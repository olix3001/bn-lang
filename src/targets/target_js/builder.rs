use std::{cell::RefCell, marker::PhantomData, rc::Rc};

use either::Either;
use once_cell::sync::Lazy;
use oxc::{
    ast::{
        AstBuilder,
        ast::{
            Argument, AssignmentOperator, AssignmentTarget, BinaryOperator, BindingPattern,
            BindingPatternKind, BindingRestElement, BlockStatement, Declaration, ExportSpecifier,
            Expression, FormalParameter, FormalParameterKind, FormalParameters, Function,
            FunctionBody, FunctionType, ImportOrExportKind, LogicalOperator, MemberExpression,
            ModuleExportName, NumberBase, ObjectProperty, ObjectPropertyKind, Program, PropertyKey,
            PropertyKind, Statement, TSThisParameter, TSTypeAnnotation, TSTypeParameterDeclaration,
            TSTypeParameterInstantiation, UnaryOperator, VariableDeclaration,
            VariableDeclarationKind, VariableDeclarator, WithClause,
        },
    },
    codegen::{Codegen, CodegenOptions},
    span::{Atom, SourceType, Span},
};
use oxc_allocator::Allocator;

pub(super) static OXC_ALLOCATOR: Lazy<Allocator> = Lazy::new(|| Allocator::new());

// Utilities
pub fn binding_ident<'a>(builder: AstBuilder<'a>, name: &str) -> BindingPattern<'a> {
    let atom = builder.atom(name);
    builder.binding_pattern::<Option<TSTypeAnnotation<'a>>>(
        builder.binding_pattern_kind_binding_identifier(Span::default(), atom),
        None,
        false,
    )
}

pub struct ObjectBindingPattern<'a> {
    pub name: String,
    pub default: Option<Expression<'a>>,
}

pub fn binding_assignment<'a>(
    builder: AstBuilder<'a>,
    name: &str,
    value: Expression<'a>,
) -> BindingPattern<'a> {
    builder.binding_pattern::<Option<TSTypeAnnotation<'a>>>(
        builder.binding_pattern_kind_assignment_pattern(
            Span::default(),
            binding_ident(builder, name),
            value,
        ),
        None,
        false,
    )
}

pub fn binding_object<'a>(
    builder: AstBuilder<'a>,
    fields: impl Iterator<Item = impl Into<ObjectBindingPattern<'a>>>,
    rest: Option<&str>,
) -> BindingPattern<'a> {
    builder.binding_pattern::<Option<TSTypeAnnotation<'a>>>(
        builder.binding_pattern_kind_object_pattern(
            Span::default(),
            builder.vec_from_iter(fields.map(|f| {
                let field: ObjectBindingPattern = f.into();
                let atom = builder.atom(&field.name);
                builder.binding_property(
                    Span::default(),
                    builder.property_key_static_identifier(Span::default(), atom),
                    match field.default {
                        Some(default) => binding_assignment(builder.clone(), &field.name, default),
                        None => binding_ident(builder.clone(), &field.name),
                    },
                    false,
                    false,
                )
            })),
            rest.map(|rest_name| {
                builder.alloc_binding_rest_element(
                    Span::default(),
                    binding_ident(builder.clone(), rest_name),
                )
            }),
        ),
        None,
        false,
    )
}

pub struct JsBuilder<'a> {
    builder: AstBuilder<'a>,
    pub(super) body: Rc<RefCell<oxc_allocator::Vec<'a, Statement<'a>>>>,
    phantom: PhantomData<&'a ()>,
}

impl<'a> Clone for JsBuilder<'a> {
    fn clone(&self) -> Self {
        Self {
            builder: self.builder.clone(),
            body: Rc::clone(&self.body),
            phantom: PhantomData,
        }
    }
}

impl<'a> Default for JsBuilder<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> JsBuilder<'a> {
    pub fn new() -> Self {
        let builder = AstBuilder::new(&OXC_ALLOCATOR);
        Self {
            body: Rc::new(RefCell::new(builder.vec())),
            builder,
            phantom: PhantomData,
        }
    }

    // Utilities for expressions/statements
    pub fn expr_stmt(&mut self, expr: Expression<'a>) {
        self.body.borrow_mut().push(Statement::ExpressionStatement(
            self.builder
                .alloc_expression_statement(Span::default(), expr),
        ))
    }

    pub fn export_name(&mut self, values: impl Iterator<Item = impl Into<ModuleExportName<'a>>>) {
        let stmt = Statement::ExportNamedDeclaration(
            self.builder
                .alloc_export_named_declaration::<Option<oxc_allocator::Box<WithClause>>>(
                    Span::default(),
                    None,
                    self.builder.vec_from_iter(values.map(|v| {
                        let name: ModuleExportName<'a> = v.into();
                        self.builder.export_specifier(
                            Span::default(),
                            name.clone(),
                            name,
                            ImportOrExportKind::Value,
                        )
                    })),
                    None,
                    ImportOrExportKind::Value,
                    None,
                ),
        );
        self.body.borrow_mut().push(stmt);
    }
    pub fn export_declaration(&mut self, declaration: impl Into<Declaration<'a>>) {
        let stmt = Statement::ExportNamedDeclaration(
            self.builder
                .alloc_export_named_declaration::<Option<oxc_allocator::Box<WithClause>>>(
                    Span::default(),
                    Some(declaration.into()),
                    self.builder.vec(),
                    None,
                    ImportOrExportKind::Value,
                    None,
                ),
        );
        self.body.borrow_mut().push(stmt);
    }

    // Variable declarations
    pub fn internal_var_decl(
        &mut self,
        kind: VariableDeclarationKind,
        name: &str,
        init_opt: Option<Expression<'a>>,
    ) -> Declaration<'a> {
        let declarator = VariableDeclarator {
            span: Span::default(),
            id: binding_ident(self.builder.clone(), name),
            init: init_opt,
            kind,
            definite: false,
        };

        Declaration::VariableDeclaration(self.builder.alloc_variable_declaration(
            Span::default(),
            kind,
            self.builder.vec_from_array([declarator]),
            false,
        ))
    }
    pub fn const_decl(&mut self, name: impl AsRef<str>, init: Expression<'a>) {
        let stmt: Statement<'a> = self
            .internal_var_decl(VariableDeclarationKind::Const, name.as_ref(), Some(init))
            .into();
        self.body.borrow_mut().push(stmt);
    }
    pub fn var_decl(&mut self, name: impl AsRef<str>, init: Option<Expression<'a>>) {
        let stmt: Statement<'a> = self
            .internal_var_decl(VariableDeclarationKind::Var, name.as_ref(), init)
            .into();
        self.body.borrow_mut().push(stmt);
    }
    pub fn let_decl(&mut self, name: impl AsRef<str>, init: Option<Expression<'a>>) {
        let stmt: Statement<'a> = self
            .internal_var_decl(VariableDeclarationKind::Let, name.as_ref(), init)
            .into();
        self.body.borrow_mut().push(stmt);
    }

    pub fn return_(&mut self, value: Option<Expression<'a>>) {
        self.body.borrow_mut().push(Statement::ReturnStatement(
            self.builder.alloc_return_statement(Span::default(), value),
        ))
    }

    pub fn code_block(&mut self, body: JsBuilder<'a>) {
        self.body.borrow_mut().push(Statement::BlockStatement(
            self.builder.alloc_block_statement(
                Span::default(),
                self.builder.vec_from_iter(body.body.borrow_mut().drain(..)),
            ),
        ))
    }

    pub fn assignment(
        &mut self,
        target: AssignmentTarget<'a>,
        op: AssignmentOperator,
        value: Expression<'a>,
    ) -> Expression<'a> {
        Expression::AssignmentExpression(self.builder.alloc_assignment_expression(
            Span::default(),
            op,
            target,
            value,
        ))
    }

    pub fn assignment_target_name(&mut self, name: &str) -> AssignmentTarget<'a> {
        self.builder
            .simple_assignment_target_assignment_target_identifier(
                Span::default(),
                self.builder.atom(name),
            )
            .into()
    }

    // Standard expressions
    pub fn static_member(
        &mut self,
        object: Expression<'a>,
        prop: &str,
        optional: bool,
    ) -> MemberExpression<'a> {
        MemberExpression::StaticMemberExpression(
            self.builder.alloc_static_member_expression(
                Span::default(),
                object,
                self.builder
                    .identifier_name(Span::default(), self.builder.atom(prop)),
                optional,
            ),
        )
    }

    pub fn computed_member(
        &mut self,
        object: Expression<'a>,
        prop: Expression<'a>,
        optional: bool,
    ) -> MemberExpression<'a> {
        MemberExpression::ComputedMemberExpression(self.builder.alloc_computed_member_expression(
            Span::default(),
            object,
            prop,
            optional,
        ))
    }

    pub fn call(
        &mut self,
        callee: Expression<'a>,
        arguments: impl Iterator<Item = Expression<'a>>,
        optional: bool,
    ) -> Expression<'a> {
        Expression::CallExpression(self.builder.alloc_call_expression::<Option<
            oxc_allocator::Box<'a, TSTypeParameterInstantiation<'a>>,
        >>(
            Span::default(),
            callee,
            None,
            self.builder.vec_from_iter(arguments.map(|arg| arg.into())),
            optional,
        ))
    }

    pub fn unary_op(&mut self, op: UnaryOperator, expr: Expression<'a>) -> Expression<'a> {
        Expression::UnaryExpression(
            self.builder
                .alloc_unary_expression(Span::default(), op, expr),
        )
    }
    pub fn binary_op(
        &mut self,
        lhs: Expression<'a>,
        op: BinaryOperator,
        rhs: Expression<'a>,
    ) -> Expression<'a> {
        Expression::BinaryExpression(self.builder.alloc_binary_expression(
            Span::default(),
            lhs,
            op,
            rhs,
        ))
    }
    pub fn logical_op(
        &mut self,
        lhs: Expression<'a>,
        op: LogicalOperator,
        rhs: Expression<'a>,
    ) -> Expression<'a> {
        Expression::LogicalExpression(self.builder.alloc_logical_expression(
            Span::default(),
            lhs,
            op,
            rhs,
        ))
    }

    // Literals
    pub fn number_literal(&mut self, value: f64) -> Expression<'a> {
        Expression::NumericLiteral(self.builder.alloc_numeric_literal(
            Span::default(),
            value,
            None,
            NumberBase::Float,
        ))
    }
    pub fn string_literal(&mut self, text: &str) -> Expression<'a> {
        Expression::StringLiteral(self.builder.alloc_string_literal(
            Span::default(),
            self.builder.atom(text),
            None,
        ))
    }
    pub fn variable_literal(&mut self, name: &str) -> Expression<'a> {
        Expression::Identifier(
            self.builder
                .alloc_identifier_reference(Span::default(), self.builder.atom(name)),
        )
    }

    // Object literal builders
    pub fn start_object_literal(&mut self) -> ObjectLiteralBuilder<'a> {
        let properties_vec = self.builder.vec();
        ObjectLiteralBuilder {
            builder: self.builder.clone(),
            properties: properties_vec,
        }
    }

    // Functions
    pub fn start_function_params(&mut self) -> FunctionParamsBuilder<'a> {
        FunctionParamsBuilder {
            builder: self.builder.clone(),
            params: self.builder.vec(),
        }
    }

    pub fn start_function(
        &mut self,
        name: Option<String>,
        is_async: bool,
        is_generator: bool,
        params: oxc_allocator::Vec<'a, FormalParameter<'a>>,
    ) -> FunctionBuilder<'a> {
        FunctionBuilder {
            builder: self.builder.clone(),
            name,
            params,
            is_async,
            is_generator,
            body: None,
        }
    }

    pub fn insert_function(&mut self, function: oxc_allocator::Box<'a, Function<'a>>) {
        self.body
            .borrow_mut()
            .push(Statement::FunctionDeclaration(function.into()))
    }

    // Building
    pub fn build_to_string(self, minify: bool) -> Result<String, anyhow::Error> {
        let mut options = CodegenOptions::default();
        options.minify = minify;
        let codegen = Codegen::new().with_options(options);
        let program = self.builder.program(
            Span::default(),
            SourceType::mjs(),
            "",
            self.builder.vec(),
            None,
            self.builder.vec(),
            self.builder.vec_from_iter(self.body.borrow_mut().drain(..)), // Yes, this could be better but i do not give a f
        );
        let output = codegen.build(&program);

        Ok(output.code)
    }
}

pub struct FunctionParamsBuilder<'a> {
    builder: AstBuilder<'a>,
    params: oxc_allocator::Vec<'a, FormalParameter<'a>>,
}

impl<'a> FunctionParamsBuilder<'a> {
    pub fn named_parameter(&mut self, name: impl AsRef<str>, default: Option<Expression<'a>>) {
        self.params.push(self.builder.formal_parameter(
            Span::default(),
            self.builder.vec(),
            match default {
                Some(expr) => binding_assignment(self.builder.clone(), name.as_ref(), expr),
                None => binding_ident(self.builder.clone(), name.as_ref()),
            },
            None,
            false,
            false,
        ));
    }

    pub fn object_parameter(
        &mut self,
        fields: impl Iterator<Item = ObjectBindingPattern<'a>>,
        rest: Option<&str>,
    ) {
        self.params.push(self.builder.formal_parameter(
            Span::default(),
            self.builder.vec(),
            binding_object(self.builder.clone(), fields, rest),
            None,
            false,
            false,
        ))
    }

    pub fn finish(self) -> oxc_allocator::Vec<'a, FormalParameter<'a>> {
        self.params
    }
}

pub struct FunctionBuilder<'a> {
    builder: AstBuilder<'a>,
    name: Option<String>,
    params: oxc_allocator::Vec<'a, FormalParameter<'a>>,
    is_async: bool,
    is_generator: bool,
    body: Option<Either<Expression<'a>, BlockStatement<'a>>>,
}

impl<'a> FunctionBuilder<'a> {
    pub fn with_full_body(&mut self, body: JsBuilder<'a>) {
        self.body = Some(Either::Right(self.builder.block_statement(
            Span::default(),
            self.builder.vec_from_iter(body.body.borrow_mut().drain(..)),
        )))
    }
    pub fn with_expr_body(&mut self, expr: Expression<'a>) {
        self.body = Some(Either::Left(expr))
    }

    pub fn finish_function(self, is_expression: bool) -> oxc_allocator::Box<'a, Function<'a>> {
        self.builder.alloc_function::<
            Option<oxc_allocator::Box<'a, TSTypeParameterDeclaration<'a>>>,
            Option<oxc_allocator::Box<'a, TSThisParameter<'a>>>, _,
            Option<oxc_allocator::Box<'a, TSTypeAnnotation<'a>>>, _
        >(
            Span::default(),
            match is_expression {
                true => FunctionType::FunctionExpression,
                false => FunctionType::FunctionDeclaration,
            },
            self.name.map(|name| {
                self.builder
                    .binding_identifier(Span::default(), self.builder.atom(&name))
            }),
            self.is_generator,
            self.is_async,
            false,
            None,
            None,
            self.builder
                .alloc_formal_parameters::<Option<oxc_allocator::Box<'a, BindingRestElement<'a>>>>(
                    Span::default(),
                    FormalParameterKind::UniqueFormalParameters,
                    self.params,
                    None,
                ),
            None,
            Some(
                self.builder.alloc_function_body(
                    Span::default(),
                    self.builder.vec(),
                    match self.body.expect("Function builder's .finish() called before inserting body") {
                        Either::Left(expr) => {
                            self.builder.vec_from_array([Statement::ExpressionStatement(
                                self.builder
                                    .alloc_expression_statement(Span::default(), expr),
                            )])
                        }
                        Either::Right(block) => block.body,
                    },
                ),
            ),
        )
    }

    pub fn finish_arrow(self) -> Expression<'a> {
        let arrow_fn_expr = self.builder.alloc_arrow_function_expression::<Option<
            oxc_allocator::Box<'a, TSTypeParameterDeclaration<'a>>,
        >, _, Option<oxc_allocator::Box<'a, TSTypeAnnotation<'a>>>, _>(
            Span::default(),
            self.body.as_ref().unwrap().is_left(),
            self.is_async,
            None,
            self.builder
                .alloc_formal_parameters::<Option<oxc_allocator::Box<'a, BindingRestElement<'a>>>>(
                    Span::default(),
                    FormalParameterKind::ArrowFormalParameters,
                    self.params,
                    None,
                ),
            None,
            match self.body.unwrap() {
                Either::Left(expr) => self.builder.alloc_function_body(
                    Span::default(),
                    self.builder.vec(),
                    self.builder.vec_from_array([Statement::ExpressionStatement(
                        self.builder
                            .alloc_expression_statement(Span::default(), expr),
                    )]),
                ),
                Either::Right(block) => self.builder.alloc_function_body(
                    Span::default(),
                    self.builder.vec(),
                    block.body,
                ),
            },
        );
        Expression::ArrowFunctionExpression(arrow_fn_expr)
    }
}

pub struct ObjectLiteralBuilder<'a> {
    builder: AstBuilder<'a>,
    properties: oxc_allocator::Vec<'a, ObjectPropertyKind<'a>>,
}

impl<'a> ObjectLiteralBuilder<'a> {
    pub fn ident_property(&mut self, name: impl AsRef<str>, value: Expression<'a>) {
        self.properties.push(ObjectPropertyKind::ObjectProperty(
            self.builder.alloc_object_property(
                Span::default(),
                PropertyKind::Init,
                PropertyKey::Identifier(
                    self.builder.alloc_identifier_reference(
                        Span::default(),
                        self.builder.atom(name.as_ref()),
                    ),
                ),
                value,
                false,
                false,
                false,
            ),
        ));
    }

    pub fn computed_property(&mut self, key: Expression<'a>, value: Expression<'a>) {
        self.properties.push(ObjectPropertyKind::ObjectProperty(
            self.builder.alloc_object_property(
                Span::default(),
                PropertyKind::Init,
                key.into(),
                value,
                false,
                false,
                false,
            ),
        ))
    }

    pub fn finish(self) -> Expression<'a> {
        Expression::ObjectExpression(
            self.builder
                .alloc_object_expression(Span::default(), self.properties),
        )
    }
}

#[cfg(test)]
mod test {
    use oxc::{
        ast::ast::{FormalParameterKind, ModuleExportName, VariableDeclarationKind},
        span::Span,
    };

    use crate::targets::target_js::builder::JsBuilder;

    #[test]
    fn test_js_builder_example() {
        let mut builder = JsBuilder::new();

        let params = builder.start_function_params().finish();
        let mut main_fn = builder.start_function(None, false, false, params);
        main_fn.with_full_body(JsBuilder::new());
        let main_fn = main_fn.finish_arrow();
        let main_call = builder.call(main_fn, vec![].into_iter(), false);

        builder.expr_stmt(main_call);

        let result = builder.build_to_string(false);
        panic!("{result:?}");
    }
}
