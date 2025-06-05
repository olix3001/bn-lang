use oxc::span::SourceType;

use crate::{
    ast::{self, Ast, BinaryOp, NodeId, NodeKind},
    error::{CompilationError, CompilerErrorType},
    lexer::Loc,
    passes::{
        Visitor,
        item_name_binding::FunctionParams,
        name_binding::{MangledName, TargetReference},
        walk_block, walk_const_decl, walk_export, walk_function_data, walk_function_def,
        walk_import_tree, walk_let_decl, walk_member_access, walk_node,
    },
    targets::{
        Target,
        target_js::builder::{JsBuilder, ObjectBindingPattern, ObjectLiteralBuilder},
    },
    utils::ComponentStorage,
};

mod builder;

pub struct JsTarget {
    pub minify: bool,
}

impl Target for JsTarget {
    type Output = String;
    type Options = ();

    fn build(
        &mut self,
        options: Self::Options,
        ast: &ast::Ast,
        component_storage: &ComponentStorage,
        root: NodeId,
    ) -> Result<Self::Output, CompilationError> {
        let mut backend = JsBackend::new(component_storage);
        backend.emit(ast, root)?;
        backend.finish(self.minify)
    }
}

const BN_RUNTIME: &'static str = include_str!("./runtime.js");

#[derive(Default)]
struct JsScope<'a> {
    builder: JsBuilder<'a>,
}

struct JsBackend<'a> {
    stack: Vec<JsScope<'a>>,
    component_storage: &'a ComponentStorage,
    current_node: NodeId,

    global_builder: JsBuilder<'a>,
    main_builder: JsBuilder<'a>,

    modules_decl: ObjectLiteralBuilder<'a>,
    root: Option<String>,
}

impl<'a> JsBackend<'a> {
    pub fn new(component_storage: &'a ComponentStorage) -> Self {
        Self {
            stack: Vec::new(),
            component_storage,
            current_node: NodeId(usize::MAX),

            global_builder: JsBuilder::new(),
            main_builder: JsBuilder::new(),

            modules_decl: JsBuilder::new().start_object_literal(),
            root: None,
        }
    }

    pub fn emit(&mut self, ast: &Ast, root: NodeId) -> Result<(), CompilationError> {
        self.traverse(ast, root)?;
        Ok(())
    }

    pub fn finish(mut self, minify: bool) -> Result<String, CompilationError> {
        // Wrap everything with (() => {})()
        let main_function_params = self.global_builder.start_function_params().finish();
        let mut main_function =
            self.global_builder
                .start_function(None, false, false, main_function_params);

        let mut full_body = JsBuilder::new();
        // Prepend modules
        full_body.const_decl("__bn_modules", self.modules_decl.finish());

        // Root module reference.
        {
            let modules_root_name = self
                .main_builder
                .string_literal(&self.root.expect("There should be default module."));
            self.main_builder
                .const_decl("__bn_root_module_id__", modules_root_name.into());
        }

        // Insert bn runtime code.
        {
            use oxc::parser::Parser;
            let body = Parser::new(&builder::OXC_ALLOCATOR, BN_RUNTIME, SourceType::cjs()).parse();
            self.main_builder
                .body
                .borrow_mut()
                .extend(body.program.body);
        }

        // Then insert body of top level code.
        full_body
            .body
            .borrow_mut()
            .extend(self.main_builder.body.borrow_mut().drain(..));
        main_function.with_full_body(full_body);

        let main_function = main_function.finish_arrow();
        let main_function_call = self
            .global_builder
            .call(main_function, [].into_iter(), false);
        self.global_builder.expr_stmt(main_function_call);

        // Build code to string.
        self.global_builder.build_to_string(minify).map_err(|err| {
            CompilationError::new(
                CompilerErrorType::InternalCompilerError(format!(
                    "Error occured during javascript codegen: {err}"
                )),
                Loc::new(0, 0..0),
            )
        })
    }

    #[track_caller]
    fn mangled_name(&self, node: NodeId) -> &str {
        self.component_storage
            .fetch::<MangledName>(node)
            .unwrap()
            .as_str()
    }

    #[inline(always)]
    fn current_mangled_name(&self) -> &str {
        self.mangled_name(self.current_node)
    }
}

#[derive(Debug)]
enum ExprOrStmt<'a> {
    Expression(oxc::ast::ast::Expression<'a>),
    Statement(oxc::ast::ast::Statement<'a>),
    Neither,
}

impl<'a> Visitor<JsScope<'a>> for JsBackend<'a> {
    type Result = Result<ExprOrStmt<'a>, CompilationError>;

    fn get_stack(&self) -> &Vec<JsScope<'a>> {
        &self.stack
    }
    fn get_stack_mut(&mut self) -> &mut Vec<JsScope<'a>> {
        &mut self.stack
    }

    fn default_result(&self) -> Self::Result {
        Ok(ExprOrStmt::Neither)
    }

    fn visit_node(&mut self, ast: &Ast, node_id: NodeId) -> Self::Result {
        // Track in which node we are. We need this for proper binding.
        let prev = self.current_node;
        self.current_node = node_id;
        let r = walk_node(self, ast, node_id);
        self.current_node = prev;
        r
    }

    fn visit_import_tree(&mut self, ast: &Ast, tree: &ast::ImportTree) -> Self::Result {
        let current_node = self.current_node;
        if matches!(tree.kind, ast::ImportTreeKind::Leaf) {
            let imported_name = self.current_mangled_name().to_string();
            let target_ref = self
                .component_storage
                .fetch::<TargetReference>(current_node)
                .unwrap();
            let target_module_name = self.mangled_name(target_ref.module.unwrap()).to_string();

            let builder = &mut self.peek_stack_mut().builder;
            let bn_require = builder.variable_literal("__bn_require__");
            let target_import_name = builder.string_literal(&target_module_name);
            let import_stmt = builder.call(bn_require, [target_import_name].into_iter(), false);

            builder.const_decl(imported_name, import_stmt);
        }
        walk_import_tree(self, ast, tree)
    }

    fn visit_module_def(
        &mut self,
        ast: &ast::Ast,
        module: &ast::Module,
        _node_loc: Loc,
    ) -> Self::Result {
        self.push_stack(JsScope::default());
        let value = self.visit_module_data(ast, module);
        let mut assembled_module = self.pop_stack().unwrap();
        if module.name.is_none() {
            self.root = Some(self.current_mangled_name().to_string());
        }

        {
            // Build function from this module.
            let mut mod_fn_params = assembled_module.builder.start_function_params();
            mod_fn_params.named_parameter("__bn_module", None);
            mod_fn_params.named_parameter("__bn_require__", None);
            let mod_fn_params = mod_fn_params.finish();
            let mut mod_fn =
                assembled_module
                    .builder
                    .start_function(None, false, false, mod_fn_params);
            mod_fn.with_full_body(assembled_module.builder);
            let mod_fn = mod_fn.finish_function(true);

            let mangled_name = self.current_mangled_name().to_string();
            self.modules_decl.ident_property(
                mangled_name,
                oxc::ast::ast::Expression::FunctionExpression(mod_fn),
            );
        }
        value
    }

    fn visit_const_decl(
        &mut self,
        ast: &Ast,
        attributes: &[ast::Attribute],
        name: NodeId,
        value: NodeId,
        _node_loc: Loc,
    ) -> Self::Result {
        if value.0 == usize::MAX {
            return self.default_result();
        };
        let mangled_name = self.current_mangled_name().to_string();

        let ExprOrStmt::Expression(init) = walk_const_decl(self, ast, attributes, name, value)?
        else {
            todo!("Can this even happen?")
        };

        let builder = &mut self.peek_stack_mut().builder;
        builder.const_decl(mangled_name, init);
        self.default_result()
    }

    fn visit_let_decl(
        &mut self,
        ast: &Ast,
        attributes: &[ast::Attribute],
        name: NodeId,
        value: Option<NodeId>,
        _node_loc: Loc,
    ) -> Self::Result {
        let mangled_name = self.current_mangled_name().to_string();

        let init = match walk_let_decl(self, ast, attributes, name, value)? {
            ExprOrStmt::Expression(expr) => Some(expr),
            _ => None,
        };

        let builder = &mut self.peek_stack_mut().builder;
        builder.let_decl(mangled_name, init);
        self.default_result()
    }

    fn visit_function_def(
        &mut self,
        ast: &Ast,
        func: &ast::Function,
        _node_loc: Loc,
    ) -> Self::Result {
        let mangled_name = self.current_mangled_name().to_string();

        let mut binding_object = Vec::new();
        for param in func.params.iter() {
            let param_name = self.mangled_name(param.name).to_string();
            let default = param
                .default_value
                .map(|v| {
                    let ExprOrStmt::Expression(expr) = self.visit_node(ast, v)? else {
                        unreachable!("Parser should not allow this")
                    };
                    Ok(expr)
                })
                .transpose()?;

            binding_object.push(ObjectBindingPattern {
                name: param_name,
                default,
            });
        }
        let mut builder = self.peek_stack().builder.clone();

        let mut params_builder = builder.start_function_params();
        params_builder.object_parameter(binding_object.into_iter(), None);
        let mut fn_builder = builder.start_function(
            Some(mangled_name),
            func.is_async,
            func.is_generator,
            params_builder.finish(),
        );

        self.push_stack(JsScope::default());
        walk_function_def(self, ast, func)?;
        let body_builder = self.pop_stack().unwrap();
        println!("body: {:?}", body_builder.builder.body);

        fn_builder.with_full_body(body_builder.builder);
        builder.insert_function(fn_builder.finish_function(false));

        self.default_result()
    }

    fn visit_function_data(&mut self, ast: &Ast, func: &ast::Function) -> Self::Result {
        walk_function_data(self, ast, func, false)
    }

    fn visit_block(
        &mut self,
        ast: &Ast,
        stmts: &[NodeId],
        trailing_expr: Option<NodeId>,
        _node_loc: Loc,
    ) -> Self::Result {
        self.push_stack(JsScope::default());
        walk_block(self, ast, stmts, trailing_expr, false)?;
        let body = self.pop_stack().unwrap();
        let builder = &mut self.peek_stack_mut().builder;
        builder.code_block(body.builder);
        self.default_result()
    }

    fn visit_return_stmt(
        &mut self,
        ast: &Ast,
        _attributes: &[ast::Attribute],
        value: Option<NodeId>,
        _node_loc: Loc,
    ) -> Self::Result {
        let value = value
            .map(|v| {
                let ExprOrStmt::Expression(expr) = self.visit_node(ast, v)? else {
                    todo!()
                };
                Ok(expr)
            })
            .transpose()?;
        let builder = &mut self.peek_stack_mut().builder;
        builder.return_(value);
        self.default_result()
    }

    fn visit_number_lit(&mut self, _ast: &Ast, value: &String, _node_loc: Loc) -> Self::Result {
        let builder = &mut self.peek_stack_mut().builder;
        let number_expr = builder.number_literal(value.parse().unwrap()); // TODO: Better number parsing.
        Ok(ExprOrStmt::Expression(number_expr))
    }

    fn visit_string_lit(&mut self, _ast: &Ast, value: &String, _node_loc: Loc) -> Self::Result {
        let builder = &mut self.peek_stack_mut().builder;
        let string_expr = builder.string_literal(value);
        Ok(ExprOrStmt::Expression(string_expr))
    }

    fn visit_boolean_lit(&mut self, _ast: &Ast, value: bool, _node_loc: Loc) -> Self::Result {
        let builder = &mut self.peek_stack_mut().builder;
        let boolean_expr = builder.boolean_literal(value);
        Ok(ExprOrStmt::Expression(boolean_expr))
    }

    fn visit_export(
        &mut self,
        ast: &Ast,
        stmt: NodeId,
        name: Option<NodeId>,
        _node_loc: Loc,
    ) -> Self::Result {
        let current_node = self.current_node;
        walk_export(self, ast, stmt, name)?;
        let target_ref = self
            .component_storage
            .fetch::<TargetReference>(current_node)
            .expect("This should be bound in name_binding pass");
        let target_name = self.mangled_name(target_ref.node).to_string(); // This is always local.

        let builder = &mut self.peek_stack_mut().builder;
        let assignment_target = builder.variable_literal("__bn_module");
        let assignment_target = builder.static_member(assignment_target, "exports", false);
        let assignment_target =
            builder.static_member(assignment_target.into(), &target_name, false);
        let assigment_value = builder.variable_literal(&target_name);
        let assigment = builder.assignment(
            assignment_target.into(),
            oxc::ast::ast::AssignmentOperator::Assign,
            assigment_value,
        );
        builder.expr_stmt(assigment);

        self.default_result()
    }

    fn visit_unary_op(
        &mut self,
        ast: &Ast,
        op: ast::UnaryOp,
        operand: NodeId,
        _node_loc: Loc,
    ) -> Self::Result {
        let ExprOrStmt::Expression(expr) = self.visit_node(ast, operand)? else {
            todo!()
        };

        let builder = &mut self.peek_stack_mut().builder;

        use oxc::ast::ast::UnaryOperator;
        Ok(ExprOrStmt::Expression(builder.unary_op(
            match op {
                ast::UnaryOp::Neg => UnaryOperator::UnaryNegation,
                ast::UnaryOp::Not => UnaryOperator::LogicalNot,
                _ => todo!("Figure out how to compile increment/decrement operators"),
            },
            expr,
        )))
    }

    fn visit_expr_stmt(&mut self, ast: &Ast, expr: NodeId, _node_loc: Loc) -> Self::Result {
        let ExprOrStmt::Expression(expr) = self.visit_node(ast, expr)? else {
            todo!()
        };
        self.peek_stack_mut().builder.expr_stmt(expr);
        self.default_result()
    }

    fn visit_binary_op(
        &mut self,
        ast: &Ast,
        op: ast::BinaryOp,
        left: NodeId,
        right: NodeId,
        _node_loc: Loc,
    ) -> Self::Result {
        let ExprOrStmt::Expression(lhs) = self.visit_node(ast, left)? else {
            todo!()
        };
        let ExprOrStmt::Expression(rhs) = self.visit_node(ast, right)? else {
            todo!()
        };

        let builder = &mut self.peek_stack_mut().builder;

        // Special case for logical operators.
        use oxc::ast::ast::{BinaryOperator, LogicalOperator};
        if matches!(op, ast::BinaryOp::And | ast::BinaryOp::Or) {
            return Ok(ExprOrStmt::Expression(builder.logical_op(
                lhs,
                match op {
                    ast::BinaryOp::And => LogicalOperator::And,
                    ast::BinaryOp::Or => LogicalOperator::Or,

                    _ => unreachable!("This is handled by matches! call above."),
                },
                rhs,
            )));
        }

        Ok(ExprOrStmt::Expression(builder.binary_op(
            lhs,
            match op {
                ast::BinaryOp::Add => BinaryOperator::Addition,
                ast::BinaryOp::Sub => BinaryOperator::Subtraction,
                ast::BinaryOp::Mul => BinaryOperator::Multiplication,
                ast::BinaryOp::Div => BinaryOperator::Division,
                ast::BinaryOp::Mod => BinaryOperator::Remainder,

                ast::BinaryOp::EqEq => BinaryOperator::StrictEquality,
                ast::BinaryOp::NotEq => BinaryOperator::StrictInequality,
                ast::BinaryOp::AboutEq => BinaryOperator::Equality,
                ast::BinaryOp::Less => BinaryOperator::LessThan,
                ast::BinaryOp::LessEq => BinaryOperator::LessEqualThan,
                ast::BinaryOp::Greater => BinaryOperator::GreaterThan,
                ast::BinaryOp::GreaterEq => BinaryOperator::GreaterEqualThan,

                ast::BinaryOp::BitwiseAnd => BinaryOperator::BitwiseAnd,
                ast::BinaryOp::BitwiseOr => BinaryOperator::BitwiseOR,
                ast::BinaryOp::BitwiseXor => BinaryOperator::BitwiseXOR,

                ast::BinaryOp::ShiftLeft => BinaryOperator::ShiftLeft,
                ast::BinaryOp::ShiftRight => BinaryOperator::ShiftRight,

                _ => todo!("Unsupported operator {op} in javascript target."),
            },
            rhs,
        )))
    }

    fn visit_call(
        &mut self,
        ast: &Ast,
        callee: NodeId,
        args: &[ast::Arg],
        node_loc: Loc,
    ) -> Self::Result {
        let callee_references = self.component_storage.fetch::<TargetReference>(callee); // TODO: Check reference in the correct way.
        let call_order_signature = callee_references
            .map(|callee_references| {
                self.component_storage
                    .fetch::<FunctionParams>(callee_references.node)
            })
            .flatten();

        let ExprOrStmt::Expression(callee) = self.visit_node(ast, callee)? else {
            unreachable!()
        };

        if let Some(call_order_signature) = call_order_signature {
            let mut builder = self.peek_stack().builder.clone();
            let mut named_args = builder.start_object_literal();
            for (i, arg) in args.iter().enumerate() {
                let ExprOrStmt::Expression(arg_expr) = self.visit_arg(ast, arg)? else {
                    unreachable!()
                };
                let arg_name = match &arg.name {
                    Some(name) => {
                        let param_ref = call_order_signature
                            .iter()
                            .find(|param| ast.ident_name(param.name) == ast.ident_name(*name))
                            .ok_or(CompilationError::new(
                                CompilerErrorType::UndefinedMember {
                                    object_type: "function".to_string(),
                                    member_name: ast.ident_name(*name),
                                },
                                node_loc.clone(),
                            ))?;
                        self.mangled_name(param_ref.name).to_string()
                    }
                    None => self.mangled_name(call_order_signature[i].name).to_string(),
                };

                named_args.ident_property(arg_name, arg_expr);
            }
            let call_expr = builder.call(callee, [named_args.finish()].into_iter(), false);

            Ok(ExprOrStmt::Expression(call_expr))
        } else {
            let args = args
                .iter()
                .map(|arg| {
                    let ExprOrStmt::Expression(arg_expr) = self.visit_arg(ast, arg)? else {
                        unreachable!()
                    };

                    Ok(arg_expr)
                })
                .collect::<Result<Vec<_>, _>>()?;

            let builder = &mut self.peek_stack_mut().builder;
            let call_expr = builder.call(callee, args.into_iter(), false);

            Ok(ExprOrStmt::Expression(call_expr))
        }
    }

    fn visit_member_access(
        &mut self,
        ast: &Ast,
        object: NodeId,
        member: NodeId,
        computed: bool,
        _node_loc: Loc,
    ) -> Self::Result {
        let ExprOrStmt::Expression(object) = self.visit_node(ast, object)? else {
            unreachable!()
        };

        match computed {
            true => {
                let ExprOrStmt::Expression(member) = self.visit_node(ast, member)? else {
                    unreachable!()
                };
                let builder = &mut self.peek_stack_mut().builder;
                let member_expr = builder.computed_member(object, member, true);
                Ok(ExprOrStmt::Expression(member_expr.into()))
            }
            false => {
                let current_node = self.current_node;
                let member_name = self.mangled_name(current_node).to_string();
                let builder = &mut self.peek_stack_mut().builder;
                let member_expr = builder.static_member(object, &member_name, true);
                Ok(ExprOrStmt::Expression(member_expr.into()))
            }
        }
    }

    fn visit_path(&mut self, ast: &Ast, segments: &[NodeId], _node_loc: Loc) -> Self::Result {
        let current_node = self.current_node;
        println!(
            "Looking for reference on path {} ({current_node})",
            segments
                .iter()
                .map(|seg| ast.ident_name(*seg))
                .collect::<Vec<_>>()
                .join("::")
        );
        let target_ref = self
            .component_storage
            .fetch::<TargetReference>(current_node)
            .expect("This should be bound in name_binding pass");

        let target_name = self.mangled_name(target_ref.node).to_string();

        if target_ref.is_local() {
            let builder = &mut self.peek_stack_mut().builder;
            return Ok(ExprOrStmt::Expression(
                builder.variable_literal(&target_name),
            ));
        } else {
            let module_import_name = self.mangled_name(target_ref.module.unwrap()).to_string();

            if matches!(
                ast.get_node(target_ref.module.unwrap()).unwrap().kind,
                NodeKind::ImportDef(..)
            ) {
                let builder = &mut self.peek_stack_mut().builder;
                let target_module = builder.variable_literal(&module_import_name);
                let reference_literal = builder.static_member(target_module, &target_name, false);

                return Ok(ExprOrStmt::Expression(reference_literal.into()));
            } else {
                // Use direclty imported value.
                let resolved_module_name =
                    self.mangled_name(target_ref.module.unwrap()).to_string();
                let builder = &mut self.peek_stack_mut().builder;
                let bn_require = builder.variable_literal("__bn_require__");
                let target_import_name = builder.string_literal(&resolved_module_name);
                let import_stmt = builder.call(bn_require, [target_import_name].into_iter(), false);

                return Ok(ExprOrStmt::Expression(
                    builder
                        .static_member(import_stmt, &target_name, false)
                        .into(),
                ));
            }
        }
    }
}
