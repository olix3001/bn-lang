use derive_more::{Deref, DerefMut};
use rustc_hash::FxHashMap;

use crate::{
    ast::{self, NodeId},
    error::CompilationError,
    passes::*,
    utils::ComponentStorage,
};

type NameStack = FxHashMap<String, NodeId>;

pub struct ItemNameBindingPass<'a> {
    name_stack: Vec<NameStack>,
    component_storage: &'a mut ComponentStorage,
    current_node: NodeId,
    super_module: Option<NodeId>,
}

#[derive(Debug, Default, Clone, Deref, DerefMut)]
pub struct ItemNamespace(pub NameStack);

#[derive(Debug, Default, Clone, Deref, DerefMut)]
pub struct ItemName(pub String);
#[derive(Clone, Deref)]
pub struct FunctionParams(pub Vec<Param>);

impl<'a> ItemNameBindingPass<'a> {
    pub fn new(component_storage: &'a mut ComponentStorage) -> Self {
        Self {
            name_stack: Vec::new(),
            component_storage,
            current_node: NodeId(usize::MAX),
            super_module: None,
        }
    }

    fn bind_current_name(&mut self, ast: &Ast, name: NodeId) {
        self.component_storage.insert(
            self.current_node,
            ItemName(ast.ident_name(name).to_string()),
        );
    }
}

impl<'a> Visitor<NameStack> for ItemNameBindingPass<'a> {
    type Result = Result<(), CompilationError>;

    fn get_stack(&self) -> &Vec<NameStack> {
        &self.name_stack
    }
    fn get_stack_mut(&mut self) -> &mut Vec<NameStack> {
        &mut self.name_stack
    }
    fn default_result(&self) -> Self::Result {
        Result::Ok(())
    }

    fn visit_node(&mut self, ast: &Ast, node_id: NodeId) -> Self::Result {
        // Track in which node we are. We need this for proper binding.
        let prev = self.current_node;
        self.current_node = node_id;
        let r = walk_node(self, ast, node_id);
        self.current_node = prev;
        r
    }

    fn visit_module_def(&mut self, ast: &Ast, module: &Module, _node_loc: Loc) -> Self::Result {
        let current_node = self.current_node;
        if let Some(name) = module.name {
            self.peek_stack_mut()
                .insert(ast.ident_name(name), current_node);
        }
        self.push_stack(NameStack::default());
        let prev_super = std::mem::replace(&mut self.super_module, Some(current_node));
        let value = self.visit_module_data(ast, module);
        self.super_module = prev_super;
        let mut namespace = self.pop_stack().unwrap();
        if let Some(super_module) = &self.super_module {
            namespace.insert("super".to_owned(), *super_module);
        }
        self.component_storage
            .insert(current_node, ItemNamespace(namespace));
        value
    }

    #[rustfmt::skip]
    fn visit_const_decl(&mut self, ast: &Ast, attributes: &[Attribute], name: NodeId, value: NodeId, _node_loc: Loc) -> Self::Result {
        self.bind_current_name(ast, name);
        walk_const_decl(self, ast, attributes, name, value)
    }
    #[rustfmt::skip]
    fn visit_let_decl(&mut self, ast: &Ast, attributes: &[Attribute], name: NodeId, value: Option<NodeId>, _node_loc: Loc) -> Self::Result {
        self.bind_current_name(ast, name);
        walk_let_decl(self, ast, attributes, name, value)
    }

    fn visit_function_def(&mut self, ast: &Ast, func: &Function, _node_loc: Loc) -> Self::Result {
        match &func.name {
            ast::ItemName::Identifier(name) => self.bind_current_name(ast, *name),
            _ => { /* Functions inside structs and objects are not bound. */ }
        }

        let current_node = self.current_node;
        self.component_storage
            .insert(current_node, FunctionParams(func.params.clone()));

        walk_function_def(self, ast, func)
    }

    fn visit_export(
        &mut self,
        ast: &Ast,
        stmt: NodeId,
        name: Option<NodeId>,
        _node_loc: Loc,
    ) -> Self::Result {
        let v = walk_export(self, ast, stmt, name)?;
        if let Some(exported_name) = self.component_storage.fetch::<ItemName>(stmt).cloned() {
            self.peek_stack_mut()
                .insert(exported_name.to_string(), stmt);
        }
        Ok(v)
    }
}
