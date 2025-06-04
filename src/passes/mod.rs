#![allow(clippy::too_many_arguments)] // Visitor methods can have many arguments

use std::ops::Try;

use crate::ast::*;
use crate::lexer::Loc;

pub mod item_name_binding;
pub mod name_binding;

// Helper macro for visiting Option<NodeId>
macro_rules! visit_opt_node_id {
    ($visitor:expr, $ast:expr, $node_id_opt:expr) => {
        if let Some(node_id) = $node_id_opt {
            Some($visitor.visit_node($ast, node_id))
        } else {
            None
        }
    };
}

// Helper macro for visiting Vec<NodeId>
macro_rules! visit_vec_node_id {
    ($visitor:expr, $ast:expr, $node_ids:expr) => {
        for &node_id in $node_ids {
            $visitor.visit_node($ast, node_id)?;
        }
    };
}

// Helper macro for visiting Vec<T> where T has a visit_T method
macro_rules! visit_vec_items {
    ($visitor:expr, $ast:expr, $items:expr, $visit_method:ident) => {
        for item in $items {
            $visitor.$visit_method($ast, item)?;
        }
    };
}

pub trait Visitor<S>: Sized
where
    S: Default,
{
    type Result: Try;

    // Required stack accessors
    fn get_stack(&self) -> &Vec<S>;
    fn get_stack_mut(&mut self) -> &mut Vec<S>;
    fn default_result(&self) -> Self::Result;

    // Provided stack utilities
    fn push_stack(&mut self, data: S) {
        self.get_stack_mut().push(data);
    }
    fn pop_stack(&mut self) -> Option<S> {
        self.get_stack_mut().pop()
    }
    fn peek_stack(&self) -> &S {
        self.get_stack().last().unwrap()
    }
    fn peek_stack_mut(&mut self) -> &mut S {
        self.get_stack_mut().last_mut().unwrap()
    }

    fn traverse(&mut self, ast: &Ast, root: NodeId) -> Self::Result {
        self.push_stack(S::default());
        let value = self.visit_node(ast, root);
        self.pop_stack();
        value
    }

    // Main entry point for visiting a node
    fn visit_node(&mut self, ast: &Ast, node_id: NodeId) -> Self::Result {
        walk_node(self, ast, node_id)
    }

    // Methods for NodeKind variants
    fn visit_module_def(&mut self, ast: &Ast, module: &Module, _node_loc: Loc) -> Self::Result {
        walk_module_def(self, ast, module)
    }
    fn visit_number_lit(&mut self, _ast: &Ast, _value: &String, _node_loc: Loc) -> Self::Result {
        // Terminal node, default walk is no-op
        self.default_result()
    }
    fn visit_string_lit(&mut self, _ast: &Ast, _value: &String, _node_loc: Loc) -> Self::Result {
        // Terminal node
        self.default_result()
    }
    fn visit_template_lit(
        &mut self,
        ast: &Ast,
        parts: &[TemplatePart],
        _node_loc: Loc,
    ) -> Self::Result {
        walk_template_lit(self, ast, parts)
    }
    fn visit_boolean_lit(&mut self, _ast: &Ast, _value: bool, _node_loc: Loc) -> Self::Result {
        // Terminal node
        self.default_result()
    }
    fn visit_null_lit(&mut self, _ast: &Ast, _node_loc: Loc) -> Self::Result {
        // Terminal node
        self.default_result()
    }
    fn visit_undefined_lit(&mut self, _ast: &Ast, _node_loc: Loc) -> Self::Result {
        // Terminal node
        self.default_result()
    }
    fn visit_array_lit(&mut self, ast: &Ast, elements: &[NodeId], _node_loc: Loc) -> Self::Result {
        walk_array_lit(self, ast, elements)
    }
    fn visit_struct_lit(
        &mut self,
        ast: &Ast,
        name: Option<NodeId>,
        fields: &[(StructLitFieldKey, NodeId)],
        _struct_lit_loc: Loc, // Location from StructLit itself
        _node_loc: Loc,       // Location from the Node
    ) -> Self::Result {
        walk_struct_lit(self, ast, name, fields)
    }
    fn visit_identifier(&mut self, _ast: &Ast, _name: &String, _node_loc: Loc) -> Self::Result {
        // Terminal node (often handled by context, e.g. Path)
        self.default_result()
    }
    fn visit_path(&mut self, ast: &Ast, segments: &[NodeId], _node_loc: Loc) -> Self::Result {
        walk_path(self, ast, segments)
    }
    fn visit_binary_op(
        &mut self,
        ast: &Ast,
        _op: BinaryOp,
        left: NodeId,
        right: NodeId,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_binary_op(self, ast, left, right)
    }
    fn visit_unary_op(
        &mut self,
        ast: &Ast,
        _op: UnaryOp,
        operand: NodeId,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_unary_op(self, ast, operand)
    }
    fn visit_call(
        &mut self,
        ast: &Ast,
        callee: NodeId,
        args: &[Arg],
        tail_closure: Option<NodeId>,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_call(self, ast, callee, args, tail_closure)
    }
    fn visit_member_access(
        &mut self,
        ast: &Ast,
        object: NodeId,
        member: NodeId,
        _computed: bool,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_member_access(self, ast, object, member)
    }
    fn visit_await(&mut self, ast: &Ast, expr: NodeId, _node_loc: Loc) -> Self::Result {
        walk_await(self, ast, expr)
    }
    fn visit_block(
        &mut self,
        ast: &Ast,
        stmts: &[NodeId],
        trailing_expr: Option<NodeId>,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_block(self, ast, stmts, trailing_expr)
    }
    fn visit_export(
        &mut self,
        ast: &Ast,
        stmt: NodeId,
        name: Option<NodeId>,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_export(self, ast, stmt, name)
    }
    fn visit_if_expr(
        &mut self,
        ast: &Ast,
        cond: NodeId,
        then_branch: NodeId,
        else_branch: Option<NodeId>,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_if_expr(self, ast, cond, then_branch, else_branch)
    }
    fn visit_loop_expr(
        &mut self,
        ast: &Ast,
        label: Option<NodeId>,
        block: NodeId,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_loop_expr(self, ast, label, block)
    }
    fn visit_while_expr(
        &mut self,
        ast: &Ast,
        label: Option<NodeId>,
        cond: NodeId,
        block: NodeId,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_while_expr(self, ast, label, cond, block)
    }
    fn visit_for_expr(
        &mut self,
        ast: &Ast,
        label: Option<NodeId>,
        var_name: NodeId,
        iterator: NodeId,
        block: NodeId,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_for_expr(self, ast, label, var_name, iterator, block)
    }
    fn visit_match_expr(
        &mut self,
        ast: &Ast,
        expr: NodeId,
        arms: &[MatchArm],
        _node_loc: Loc,
    ) -> Self::Result {
        walk_match_expr(self, ast, expr, arms)
    }
    fn visit_closure_expr(
        &mut self,
        ast: &Ast,
        params: &[Param],
        body: NodeId,
        _closure_loc: Loc, // Location from ClosureExpr itself
        _node_loc: Loc,    // Location from the Node
    ) -> Self::Result {
        walk_closure_expr(self, ast, params, body)
    }
    fn visit_let_decl(
        &mut self,
        ast: &Ast,
        attributes: &[Attribute],
        name: NodeId,
        value: Option<NodeId>,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_let_decl(self, ast, attributes, name, value)
    }
    fn visit_const_decl(
        &mut self,
        ast: &Ast,
        attributes: &[Attribute],
        name: NodeId,
        value: NodeId,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_const_decl(self, ast, attributes, name, value)
    }
    fn visit_expr_stmt(&mut self, ast: &Ast, expr: NodeId, _node_loc: Loc) -> Self::Result {
        walk_expr_stmt(self, ast, expr)
    }
    fn visit_return_stmt(
        &mut self,
        ast: &Ast,
        attributes: &[Attribute],
        value: Option<NodeId>,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_return_stmt(self, ast, attributes, value)
    }
    fn visit_yield_stmt(
        &mut self,
        ast: &Ast,
        attributes: &[Attribute],
        value: Option<NodeId>,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_yield_stmt(self, ast, attributes, value)
    }
    fn visit_break_stmt(
        &mut self,
        ast: &Ast,
        attributes: &[Attribute],
        label: Option<NodeId>,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_break_stmt(self, ast, attributes, label)
    }
    fn visit_continue_stmt(
        &mut self,
        ast: &Ast,
        attributes: &[Attribute],
        label: Option<NodeId>,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_continue_stmt(self, ast, attributes, label)
    }
    fn visit_function_def(&mut self, ast: &Ast, func: &Function, _node_loc: Loc) -> Self::Result {
        walk_function_def(self, ast, func)
    }
    fn visit_struct_def(&mut self, ast: &Ast, structure: &Struct, _node_loc: Loc) -> Self::Result {
        walk_struct_def(self, ast, structure)
    }
    fn visit_enum_def(&mut self, ast: &Ast, enume: &Enum, _node_loc: Loc) -> Self::Result {
        walk_enum_def(self, ast, enume)
    }
    fn visit_import_def(&mut self, ast: &Ast, import_def: &Import, _node_loc: Loc) -> Self::Result {
        walk_import_def(self, ast, import_def)
    }
    fn visit_label_node(
        &mut self,
        ast: &Ast,
        name: NodeId,
        item: NodeId,
        _node_loc: Loc,
    ) -> Self::Result {
        walk_label_node(self, ast, name, item)
    }
    fn visit_error_node(&mut self, _ast: &Ast, _node_loc: Loc) -> Self::Result {
        // Terminal node
        self.default_result()
    }

    // Methods for auxiliary AST structures (data within NodeKind variants or other structs)
    fn visit_module_data(&mut self, ast: &Ast, module: &Module) -> Self::Result {
        walk_module_data(self, ast, module)
    }
    fn visit_function_data(&mut self, ast: &Ast, func: &Function) -> Self::Result {
        walk_function_data(self, ast, func)
    }
    fn visit_struct_data(&mut self, ast: &Ast, structure: &Struct) -> Self::Result {
        walk_struct_data(self, ast, structure)
    }
    fn visit_enum_data(&mut self, ast: &Ast, enume: &Enum) -> Self::Result {
        walk_enum_data(self, ast, enume)
    }
    fn visit_import_data(&mut self, ast: &Ast, import_def: &Import) -> Self::Result {
        walk_import_data(self, ast, import_def)
    }

    fn visit_template_part(&mut self, ast: &Ast, part: &TemplatePart) -> Self::Result {
        walk_template_part(self, ast, part)
    }
    fn visit_struct_lit_field_key(&mut self, ast: &Ast, key: &StructLitFieldKey) -> Self::Result {
        walk_struct_lit_field_key(self, ast, key)
    }
    fn visit_arg(&mut self, ast: &Ast, arg: &Arg) -> Self::Result {
        walk_arg(self, ast, arg)
    }
    fn visit_param(&mut self, ast: &Ast, param: &Param) -> Self::Result {
        walk_param(self, ast, param)
    }
    fn visit_attribute(&mut self, ast: &Ast, attr: &Attribute) -> Self::Result {
        walk_attribute(self, ast, attr)
    }
    fn visit_item_name(&mut self, ast: &Ast, name: &ItemName) -> Self::Result {
        walk_item_name(self, ast, name)
    }
    fn visit_match_arm(&mut self, ast: &Ast, arm: &MatchArm) -> Self::Result {
        walk_match_arm(self, ast, arm)
    }
    fn visit_struct_member(&mut self, ast: &Ast, member: &StructMember) -> Self::Result {
        walk_struct_member(self, ast, member)
    }
    fn visit_enum_variant(&mut self, ast: &Ast, variant: &EnumVariant) -> Self::Result {
        walk_enum_variant(self, ast, variant)
    }
    fn visit_enum_variant_kind(&mut self, ast: &Ast, kind: &EnumVariantKind) -> Self::Result {
        walk_enum_variant_kind(self, ast, kind)
    }
    fn visit_import_tree(&mut self, ast: &Ast, tree: &ImportTree) -> Self::Result {
        walk_import_tree(self, ast, tree)
    }
    fn visit_import_group_item(&mut self, ast: &Ast, item: &ImportGroupItem) -> Self::Result {
        walk_import_group_item(self, ast, item)
    }
}

// Default traversal functions (walk_...)

pub fn walk_node<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    node_id: NodeId,
) -> R {
    if let Some(node) = ast.get_node(node_id) {
        match &node.kind {
            NodeKind::Module(module) => visitor.visit_module_def(ast, module, node.loc.clone()),
            NodeKind::NumberLit(value) => visitor.visit_number_lit(ast, value, node.loc.clone()),
            NodeKind::StringLit(value) => visitor.visit_string_lit(ast, value, node.loc.clone()),
            NodeKind::TemplateLit { parts } => {
                visitor.visit_template_lit(ast, parts, node.loc.clone())
            }
            NodeKind::BooleanLit(value) => visitor.visit_boolean_lit(ast, *value, node.loc.clone()),
            NodeKind::NullLit => visitor.visit_null_lit(ast, node.loc.clone()),
            NodeKind::UndefinedLit => visitor.visit_undefined_lit(ast, node.loc.clone()),
            NodeKind::ArrayLit(elements) => {
                visitor.visit_array_lit(ast, elements, node.loc.clone())
            }
            NodeKind::StructLit { name, fields, loc } => {
                visitor.visit_struct_lit(ast, *name, fields, loc.clone(), node.loc.clone())
            }
            NodeKind::Identifier(name) => visitor.visit_identifier(ast, name, node.loc.clone()),
            NodeKind::Path { segments } => visitor.visit_path(ast, segments, node.loc.clone()),
            NodeKind::BinaryOp { op, left, right } => {
                visitor.visit_binary_op(ast, *op, *left, *right, node.loc.clone())
            }
            NodeKind::UnaryOp { op, operand } => {
                visitor.visit_unary_op(ast, *op, *operand, node.loc.clone())
            }
            NodeKind::Call {
                callee,
                args,
                tail_closure,
            } => visitor.visit_call(ast, *callee, args, *tail_closure, node.loc.clone()),
            NodeKind::MemberAccess {
                object,
                member,
                computed,
            } => visitor.visit_member_access(ast, *object, *member, *computed, node.loc.clone()),
            NodeKind::Await { expr } => visitor.visit_await(ast, *expr, node.loc.clone()),
            NodeKind::Block {
                stmts,
                trailing_expr,
            } => visitor.visit_block(ast, stmts, *trailing_expr, node.loc.clone()),
            NodeKind::Export { stmt, name } => {
                visitor.visit_export(ast, *stmt, name.clone(), node.loc.clone())
            }
            NodeKind::IfExpr {
                cond,
                then_branch,
                else_branch,
            } => visitor.visit_if_expr(ast, *cond, *then_branch, *else_branch, node.loc.clone()),
            NodeKind::LoopExpr { label, block } => {
                visitor.visit_loop_expr(ast, *label, *block, node.loc.clone())
            }
            NodeKind::WhileExpr { label, cond, block } => {
                visitor.visit_while_expr(ast, *label, *cond, *block, node.loc.clone())
            }
            NodeKind::ForExpr {
                label,
                var_name,
                iterator,
                block,
            } => {
                visitor.visit_for_expr(ast, *label, *var_name, *iterator, *block, node.loc.clone())
            }
            NodeKind::MatchExpr { expr, arms } => {
                visitor.visit_match_expr(ast, *expr, arms, node.loc.clone())
            }
            NodeKind::ClosureExpr { params, body, loc } => {
                visitor.visit_closure_expr(ast, params, *body, loc.clone(), node.loc.clone())
            }
            NodeKind::LetDecl {
                attributes,
                name,
                value,
            } => visitor.visit_let_decl(ast, attributes, *name, *value, node.loc.clone()),
            NodeKind::ConstDecl {
                attributes,
                name,
                value,
            } => visitor.visit_const_decl(ast, attributes, *name, *value, node.loc.clone()),
            NodeKind::ExprStmt(expr) => visitor.visit_expr_stmt(ast, *expr, node.loc.clone()),
            NodeKind::ReturnStmt { attributes, value } => {
                visitor.visit_return_stmt(ast, attributes, *value, node.loc.clone())
            }
            NodeKind::YieldStmt { attributes, value } => {
                visitor.visit_yield_stmt(ast, attributes, *value, node.loc.clone())
            }
            NodeKind::BreakStmt { attributes, label } => {
                visitor.visit_break_stmt(ast, attributes, *label, node.loc.clone())
            }
            NodeKind::ContinueStmt { attributes, label } => {
                visitor.visit_continue_stmt(ast, attributes, *label, node.loc.clone())
            }
            NodeKind::FunctionDef(func) => visitor.visit_function_def(ast, func, node.loc.clone()),
            NodeKind::StructDef(structure) => {
                visitor.visit_struct_def(ast, structure, node.loc.clone())
            }
            NodeKind::EnumDef(enume) => visitor.visit_enum_def(ast, enume, node.loc.clone()),
            NodeKind::ImportDef(import_def) => {
                visitor.visit_import_def(ast, import_def, node.loc.clone())
            }
            NodeKind::LabelNode { name, item } => {
                visitor.visit_label_node(ast, *name, *item, node.loc.clone())
            }
            NodeKind::Error => visitor.visit_error_node(ast, node.loc.clone()),
        }
    } else {
        visitor.default_result()
    }
}

// Walk functions for NodeKind variants
pub fn walk_module_def<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    module: &Module,
) -> R {
    visitor.push_stack(S::default());
    let value = visitor.visit_module_data(ast, module);
    visitor.pop_stack();
    value
}

pub fn walk_template_lit<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    parts: &[TemplatePart],
) -> R {
    visit_vec_items!(visitor, ast, parts, visit_template_part);
    visitor.default_result()
}

pub fn walk_array_lit<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    elements: &[NodeId],
) -> R {
    visit_vec_node_id!(visitor, ast, elements);
    visitor.default_result()
}

pub fn walk_struct_lit<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    name: Option<NodeId>,
    fields: &[(StructLitFieldKey, NodeId)],
) -> R {
    visit_opt_node_id!(visitor, ast, name);
    for (key, value_id) in fields {
        visitor.visit_struct_lit_field_key(ast, key)?;
        visitor.visit_node(ast, *value_id)?;
    }
    visitor.default_result()
}

pub fn walk_path<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    segments: &[NodeId],
) -> R {
    visit_vec_node_id!(visitor, ast, segments);
    visitor.default_result()
}

pub fn walk_binary_op<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    left: NodeId,
    right: NodeId,
) -> R {
    visitor.visit_node(ast, left)?;
    visitor.visit_node(ast, right)
}

pub fn walk_unary_op<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    operand: NodeId,
) -> R {
    visitor.visit_node(ast, operand)
}

pub fn walk_call<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    callee: NodeId,
    args: &[Arg],
    tail_closure: Option<NodeId>,
) -> R {
    let callee = visitor.visit_node(ast, callee);
    visit_vec_items!(visitor, ast, args, visit_arg);
    visit_opt_node_id!(visitor, ast, tail_closure);
    callee
}

pub fn walk_member_access<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    object: NodeId,
    member: NodeId,
) -> R {
    visitor.visit_node(ast, object)?;
    visitor.visit_node(ast, member) // Member can be identifier or expression if computed
}

pub fn walk_await<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    expr: NodeId,
) -> R {
    visitor.visit_node(ast, expr)
}

pub fn walk_block<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    stmts: &[NodeId],
    trailing_expr: Option<NodeId>,
) -> R {
    visitor.push_stack(S::default());
    visit_vec_node_id!(visitor, ast, stmts);
    let trailing = visit_opt_node_id!(visitor, ast, trailing_expr);
    visitor.pop_stack();
    if let Some(trailing) = trailing {
        trailing
    } else {
        visitor.default_result()
    }
}

pub fn walk_export<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    stmt: NodeId,
    name: Option<NodeId>,
) -> R {
    visit_opt_node_id!(visitor, ast, name);
    visitor.visit_node(ast, stmt)
}

pub fn walk_if_expr<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    cond: NodeId,
    then_branch: NodeId,
    else_branch: Option<NodeId>,
) -> R {
    visitor.visit_node(ast, cond)?;
    visitor.visit_node(ast, then_branch)?;
    visit_opt_node_id!(visitor, ast, else_branch);
    visitor.default_result()
}

pub fn walk_loop_expr<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    label: Option<NodeId>,
    block: NodeId,
) -> R {
    visit_opt_node_id!(visitor, ast, label);
    visitor.visit_node(ast, block)?;
    visitor.default_result()
}

pub fn walk_while_expr<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    label: Option<NodeId>,
    cond: NodeId,
    block: NodeId,
) -> R {
    visit_opt_node_id!(visitor, ast, label);
    visitor.visit_node(ast, cond)?;
    visitor.visit_node(ast, block)?;
    visitor.default_result()
}

pub fn walk_for_expr<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    label: Option<NodeId>,
    var_name: NodeId,
    iterator: NodeId,
    block: NodeId,
) -> R {
    visit_opt_node_id!(visitor, ast, label);
    visitor.visit_node(ast, var_name)?;
    visitor.visit_node(ast, iterator)?;
    visitor.visit_node(ast, block)?;
    visitor.default_result()
}

pub fn walk_match_expr<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    expr: NodeId,
    arms: &[MatchArm],
) -> R {
    visitor.visit_node(ast, expr)?;
    visit_vec_items!(visitor, ast, arms, visit_match_arm);
    visitor.default_result()
}

pub fn walk_closure_expr<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    params: &[Param],
    body: NodeId,
) -> R {
    visit_vec_items!(visitor, ast, params, visit_param);
    visitor.visit_node(ast, body)
}

pub fn walk_let_decl<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    attributes: &[Attribute],
    name: NodeId,
    value: Option<NodeId>,
) -> R {
    visit_vec_items!(visitor, ast, attributes, visit_attribute);
    visitor.visit_node(ast, name)?;
    if let Some(init) = visit_opt_node_id!(visitor, ast, value) {
        init
    } else {
        visitor.default_result()
    }
}

pub fn walk_const_decl<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    attributes: &[Attribute],
    name: NodeId,
    value: NodeId,
) -> R {
    visit_vec_items!(visitor, ast, attributes, visit_attribute);
    visitor.visit_node(ast, name)?;
    visitor.visit_node(ast, value)
}

pub fn walk_expr_stmt<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    expr: NodeId,
) -> R {
    visitor.visit_node(ast, expr)
}

pub fn walk_return_stmt<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    attributes: &[Attribute],
    value: Option<NodeId>,
) -> R {
    visit_vec_items!(visitor, ast, attributes, visit_attribute);
    visit_opt_node_id!(visitor, ast, value);
    visitor.default_result()
}

pub fn walk_yield_stmt<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    attributes: &[Attribute],
    value: Option<NodeId>,
) -> R {
    visit_vec_items!(visitor, ast, attributes, visit_attribute);
    visit_opt_node_id!(visitor, ast, value);
    visitor.default_result()
}

pub fn walk_break_stmt<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    attributes: &[Attribute],
    label: Option<NodeId>,
) -> R {
    visit_vec_items!(visitor, ast, attributes, visit_attribute);
    visit_opt_node_id!(visitor, ast, label);
    visitor.default_result()
}

pub fn walk_continue_stmt<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    attributes: &[Attribute],
    label: Option<NodeId>,
) -> R {
    visit_vec_items!(visitor, ast, attributes, visit_attribute);
    visit_opt_node_id!(visitor, ast, label);
    visitor.default_result()
}

pub fn walk_function_def<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    func: &Function,
) -> R {
    visitor.visit_function_data(ast, func)
}

pub fn walk_struct_def<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    structure: &Struct,
) -> R {
    visitor.visit_struct_data(ast, structure)
}

pub fn walk_enum_def<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    enume: &Enum,
) -> R {
    visitor.visit_enum_data(ast, enume)
}

pub fn walk_import_def<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    import_def: &Import,
) -> R {
    visitor.visit_import_data(ast, import_def)
}

pub fn walk_label_node<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    name: NodeId,
    item: NodeId,
) -> R {
    visitor.visit_node(ast, name)?; // Label name (identifier)
    visitor.visit_node(ast, item) // Labeled item
}

// Walk functions for auxiliary AST structures
pub fn walk_module_data<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    module: &Module,
) -> R {
    visit_vec_items!(visitor, ast, &module.attributes, visit_attribute);
    visit_opt_node_id!(visitor, ast, module.name);
    visit_vec_node_id!(visitor, ast, &module.stmts);
    visitor.default_result()
}

pub fn walk_function_data<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    func: &Function,
) -> R {
    visit_vec_items!(visitor, ast, &func.attributes, visit_attribute);
    visitor.visit_item_name(ast, &func.name);
    visit_vec_items!(visitor, ast, &func.params, visit_param);
    visitor.visit_node(ast, func.body)?;
    visitor.default_result()
}

pub fn walk_struct_data<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    structure: &Struct,
) -> R {
    visit_vec_items!(visitor, ast, &structure.attributes, visit_attribute);
    visitor.visit_item_name(ast, &structure.name)?;
    visit_vec_items!(visitor, ast, &structure.members, visit_struct_member);
    visitor.default_result()
}

pub fn walk_enum_data<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    enume: &Enum,
) -> R {
    visit_vec_items!(visitor, ast, &enume.attributes, visit_attribute);
    visitor.visit_item_name(ast, &enume.name)?;
    visit_vec_items!(visitor, ast, &enume.variants, visit_enum_variant);
    for method_func in &enume.methods {
        visitor.visit_function_data(ast, method_func)?; // Re-use function data visiting
    }
    visitor.default_result()
}

pub fn walk_import_data<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    import_def: &Import,
) -> R {
    visit_vec_items!(visitor, ast, &import_def.attributes, visit_attribute);
    visitor.visit_import_tree(ast, &import_def.tree)
}

pub fn walk_template_part<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    part: &TemplatePart,
) -> R {
    match part {
        TemplatePart::Literal(_) => visitor.default_result(), // Terminal
        TemplatePart::Expression(expr_id) => visitor.visit_node(ast, *expr_id),
    }
}

pub fn walk_struct_lit_field_key<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    key: &StructLitFieldKey,
) -> R {
    match key {
        StructLitFieldKey::Identifier(id) => visitor.visit_node(ast, *id),
        StructLitFieldKey::Expression(id) => visitor.visit_node(ast, *id),
    }
}

pub fn walk_arg<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    arg: &Arg,
) -> R {
    visit_opt_node_id!(visitor, ast, arg.name);
    visitor.visit_node(ast, arg.value)
}

pub fn walk_param<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    param: &Param,
) -> R {
    visit_vec_items!(visitor, ast, &param.attributes, visit_attribute);
    visitor.visit_node(ast, param.name)?;
    visit_opt_node_id!(visitor, ast, param.default_value);
    visitor.default_result()
}

pub fn walk_attribute<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    attr: &Attribute,
) -> R {
    visit_vec_node_id!(visitor, ast, &attr.path);
    visitor.default_result()
    // attr.args_raw_tokens are not AST nodes, not visited by default
}

pub fn walk_item_name<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    name: &ItemName,
) -> R {
    match name {
        ItemName::Identifier(id) => visitor.visit_node(ast, *id),
        ItemName::Computed(id) => visitor.visit_node(ast, *id),
    }
}

pub fn walk_match_arm<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    arm: &MatchArm,
) -> R {
    visitor.visit_node(ast, arm.pattern)?;
    visit_opt_node_id!(visitor, ast, arm.guard);
    visitor.visit_node(ast, arm.body)
}

pub fn walk_struct_member<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    member: &StructMember,
) -> R {
    match member {
        StructMember::Field {
            attributes, name, ..
        } => {
            visit_vec_items!(visitor, ast, attributes, visit_attribute);
            visitor.visit_item_name(ast, name)
        }
        StructMember::Method(func) => {
            // Note: A method is a Function struct directly, not a NodeId to a FunctionDef.
            // We visit its data, not as a FunctionDef node.
            visitor.visit_function_data(ast, func)
        }
    }
}

pub fn walk_enum_variant<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    variant: &EnumVariant,
) -> R {
    visit_vec_items!(visitor, ast, &variant.attributes, visit_attribute);
    visitor.visit_node(ast, variant.name)?;
    visitor.visit_enum_variant_kind(ast, &variant.kind)?;
    visitor.default_result()
}

pub fn walk_enum_variant_kind<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    kind: &EnumVariantKind,
) -> R {
    match kind {
        EnumVariantKind::Simple => {} // Terminal
        EnumVariantKind::TupleStyle(node_ids) => {
            visit_vec_node_id!(visitor, ast, node_ids)
        }
        EnumVariantKind::StructStyle(params) => {
            visit_vec_items!(visitor, ast, params, visit_param)
        }
    }
    visitor.default_result()
}

pub fn walk_import_tree<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    tree: &ImportTree,
) -> R {
    visit_vec_node_id!(visitor, ast, &tree.prefix);
    match &tree.kind {
        ImportTreeKind::Leaf => {} // Terminal for the prefix
        ImportTreeKind::Group(items) => {
            visit_vec_items!(visitor, ast, items, visit_import_group_item);
        }
    }
    visitor.default_result()
}

pub fn walk_import_group_item<V: Visitor<S, Result = R>, S: Default, R: Try>(
    visitor: &mut V,
    ast: &Ast,
    item: &ImportGroupItem,
) -> R {
    match item {
        ImportGroupItem::Path(import_tree) => {
            visitor.visit_import_tree(ast, import_tree)?;
        }
        ImportGroupItem::SelfImport(_) => {} // Terminal
        ImportGroupItem::RenamedPath { tree, new_name, .. } => {
            visitor.visit_import_tree(ast, tree)?;
            visitor.visit_node(ast, *new_name)?; // The new name (identifier)
        }
    }
    visitor.default_result()
}
