use oxc::span::SourceType;

use crate::{
    ast::{self, Ast, BinaryOp, NodeId, NodeKind},
    error::{CompilationError, CompilerErrorType},
    lexer::Loc,
    passes::{
        Visitor,
        name_binding::{MangledName, TargetReference},
        walk_const_decl, walk_export, walk_import_tree, walk_let_decl, walk_node,
    },
    targets::{
        Target,
        target_js::builder::{JsBuilder, ObjectLiteralBuilder},
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

        // Insert bn runtime code.
        {
            use oxc::parser::Parser;
            let body = Parser::new(&builder::OXC_ALLOCATOR, BN_RUNTIME, SourceType::cjs()).parse();
            self.main_builder.body.extend(body.program.body);
        }

        // Then insert body of top level code.
        full_body.body.extend(self.main_builder.body);
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

    fn visit_number_lit(&mut self, _ast: &Ast, value: &String, _node_loc: Loc) -> Self::Result {
        let builder = &mut self.peek_stack_mut().builder;
        let number_expr = builder.number_literal(value.parse().unwrap()); // TODO: Better number parsing.
        Ok(ExprOrStmt::Expression(number_expr))
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

    fn visit_binary_op(
        &mut self,
        ast: &Ast,
        op: ast::BinaryOp,
        left: NodeId,
        right: NodeId,
        _node_loc: Loc,
    ) -> Self::Result {
        let ExprOrStmt::Expression(lhs) = walk_node(self, ast, left)? else {
            todo!()
        };
        let ExprOrStmt::Expression(rhs) = walk_node(self, ast, right)? else {
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

    fn visit_path(&mut self, ast: &Ast, segments: &[NodeId], _node_loc: Loc) -> Self::Result {
        let current_node = self.current_node;
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
