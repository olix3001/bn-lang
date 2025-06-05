use derive_more::Deref;
use logos::Source;
use rustc_hash::FxHashMap;

use crate::{
    ast::{self, NodeId},
    error::{CompilationError, CompilerErrorType},
    passes::{item_name_binding::ItemNamespace, *},
    utils::ComponentStorage,
};

type NameStack = FxHashMap<String, (NodeId, Option<NodeId>)>;

#[derive(Clone, PartialEq)]
pub struct TargetReference {
    pub module: Option<NodeId>,
    pub node: NodeId,
}

impl TargetReference {
    pub fn new_local(node: NodeId) -> Self {
        Self { module: None, node }
    }
    pub fn new(module: Option<NodeId>, node: NodeId) -> Self {
        Self { module, node }
    }

    #[inline(always)]
    pub fn is_local(&self) -> bool {
        self.module.is_none()
    }
}

#[derive(Clone, PartialEq, Deref)]
pub struct MangledName(pub String);

impl MangledName {
    pub fn new(name: impl AsRef<str>, loc: Loc) -> Self {
        // TODO: Implement better looking name mangling.
        use std::hash::{DefaultHasher, Hash, Hasher};
        let mut hasher = DefaultHasher::new();
        loc.hash(&mut hasher);
        Self(format!(
            "{}__${}",
            name.as_ref(),
            format!("{:x}", hasher.finish()).slice(0..5).unwrap()
        ))
    }
}

pub struct NameBindingPass<'a> {
    name_stack: Vec<NameStack>,
    scope: FxHashMap<String, NodeId>,
    component_storage: &'a mut ComponentStorage,
    current_node: NodeId,
    last_path_node: NodeId,
}

impl<'a> NameBindingPass<'a> {
    pub fn new(component_storage: &'a mut ComponentStorage) -> Self {
        Self {
            name_stack: Vec::new(),
            scope: FxHashMap::default(),
            component_storage,
            current_node: NodeId(usize::MAX),
            last_path_node: NodeId(usize::MAX),
        }
    }

    fn bind_current_mangled_name_raw(&mut self, ast: &Ast, name: &str, loc: &Loc) {
        self.component_storage.insert(
            self.current_node,
            MangledName::new(name.to_string(), loc.clone()),
        );
    }
    #[inline(always)]
    fn bind_current_mangled_name(&mut self, ast: &Ast, name: Option<NodeId>, loc: &Loc) {
        self.bind_current_mangled_name_raw(ast, ast.get_ident_name(name).unwrap_or("default"), loc)
    }

    pub fn traverse_path(
        &mut self,
        ast: &Ast,
        path: &Vec<NodeId>,
        loc: Loc,
    ) -> Result<(String, NodeId, NodeId), CompilationError> {
        // TODO: Get rid of all these .clone() calls.
        let mut current_scope = self.scope.clone();
        let mut last_module = self.last_path_node;
        let last_idx = path.len() - 1;
        for (i, segment) in path.iter().enumerate() {
            let segment_value = ast.ident_name(*segment);
            if let Some(new_scope_id) = current_scope.get(&segment_value) {
                if self.component_storage.has::<ItemNamespace>(*new_scope_id) {
                    // is module
                    last_module = *new_scope_id;
                }
                if i == last_idx {
                    self.last_path_node = last_module;
                    return Ok((segment_value, *new_scope_id, last_module));
                }
                let new_scope = self
                    .component_storage
                    .fetch::<ItemNamespace>(*new_scope_id)
                    .unwrap()
                    .0
                    .clone();
                current_scope = new_scope;
            } else {
                Err(CompilationError::new(
                    CompilerErrorType::InvalidPath {
                        item: segment_value,
                        module: "<todo>".to_string(),
                    },
                    loc.clone(),
                ))?
            }
        }

        unreachable!()
    }
}

impl<'a> Visitor<NameStack> for NameBindingPass<'a> {
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

    // Items for binding scope and resolving paths
    fn visit_module_def(&mut self, ast: &Ast, module: &Module, node_loc: Loc) -> Self::Result {
        let current_node = self.current_node;
        self.bind_current_mangled_name(ast, module.name, &node_loc);

        let Some(namespace) = self.component_storage.fetch::<ItemNamespace>(current_node) else {
            Err(CompilationError::new(
                CompilerErrorType::InternalCompilerError(
                    "Module has no binded ItemNamespace during expression name binding.".to_owned(),
                ),
                node_loc,
            ))?
        };
        let base_scope = std::mem::replace(&mut self.scope, namespace.0.clone());
        walk_module_def(self, ast, module)?;
        self.scope = base_scope;

        self.default_result()
    }

    fn visit_import_tree(&mut self, ast: &Ast, tree: &ImportTree) -> Self::Result {
        let current_node = self.current_node;
        match &tree.kind {
            ImportTreeKind::Leaf => {
                let (last_name, last_id, last_module_id) =
                    self.traverse_path(ast, &tree.prefix, tree.loc.clone())?;
                let NodeKind::Module(last_module) =
                    &ast.get_node(last_module_id).as_ref().unwrap().kind
                else {
                    unreachable!()
                };
                self.bind_current_mangled_name_raw(
                    ast,
                    &format!(
                        "__bn__{}",
                        ast.get_ident_name(last_module.name).unwrap_or("default")
                    ),
                    &tree.loc,
                );
                self.component_storage.insert(
                    current_node,
                    TargetReference::new(Some(last_module_id), last_id),
                );
                self.peek_stack_mut()
                    .insert(last_name, (last_id, Some(current_node)));
                walk_import_tree(self, ast, tree)
            }
            ImportTreeKind::Group(_) => {
                let (_, last_id, _) = self.traverse_path(ast, &tree.prefix, tree.loc.clone())?;
                let new_scope = self
                    .component_storage
                    .fetch::<ItemNamespace>(last_id)
                    .unwrap()
                    .0
                    .clone();

                let prev_scope = std::mem::replace(&mut self.scope, new_scope);
                let v = walk_import_tree(self, ast, tree)?;
                self.scope = prev_scope;
                Ok(v)
            }
        }
    }

    // Actual expressions that are being bound.
    #[rustfmt::skip]
    fn visit_const_decl(&mut self, ast: &Ast, attributes: &[Attribute], name: NodeId, value: NodeId, node_loc: Loc) -> Self::Result {
        self.bind_current_mangled_name(ast, Some(name), &node_loc);
        walk_const_decl(self, ast, attributes, name, value)?;
        let current_node = self.current_node;
        self.peek_stack_mut().insert(ast.ident_name(name), (current_node, None));
        self.default_result()
    }

    #[rustfmt::skip]
    fn visit_let_decl(&mut self, ast: &Ast, attributes: &[Attribute], name: NodeId, value: Option<NodeId>, node_loc: Loc) -> Self::Result {
        self.bind_current_mangled_name(ast, Some(name), &node_loc);
        walk_let_decl(self, ast, attributes, name, value)?;
        let current_node = self.current_node;
        self.peek_stack_mut().insert(ast.ident_name(name), (current_node, None));
        self.default_result()
    }

    fn visit_function_def(&mut self, ast: &Ast, func: &Function, node_loc: Loc) -> Self::Result {
        let ast::ItemName::Identifier(name) = &func.name else {
            todo!()
        };
        println!(
            "VISITING FUNCTION {} WITH {} PARAMS",
            ast.ident_name(*name),
            func.params.len()
        );
        self.bind_current_mangled_name(ast, Some(*name), &node_loc);
        let current_node = self.current_node;
        let def_value = walk_function_def(self, ast, func)?;
        self.peek_stack_mut()
            .insert(ast.ident_name(*name), (current_node, None));
        Ok(def_value)
    }

    fn visit_param(&mut self, ast: &Ast, param: &Param) -> Self::Result {
        let prev_node = std::mem::replace(&mut self.current_node, param.name);
        self.bind_current_mangled_name(ast, Some(param.name), &param.loc);
        let current_node = self.current_node;
        self.peek_stack_mut()
            .insert(ast.ident_name(param.name), (current_node, None));
        walk_param(self, ast, param)?;
        self.current_node = prev_node;
        self.default_result()
    }

    #[rustfmt::skip]
    fn visit_export(&mut self, ast: &Ast, stmt: NodeId, name: Option<NodeId>, node_loc: Loc) -> Self::Result {
        walk_export(self, ast, stmt, name)?;

        if self.component_storage.has::<MangledName>(stmt) { // This way we can check whether this export references anything.
            let current_node = self.current_node;
            self.component_storage.insert(current_node, TargetReference::new_local(stmt));
            self.default_result()
        } else {
            Err(CompilationError::new(
                CompilerErrorType::InvalidOperation { op: "export".to_string(), type_name: "expression or invalid statement".to_string() },
                node_loc
            ))
        }
    }

    fn visit_path(&mut self, ast: &Ast, segments: &[NodeId], node_loc: Loc) -> Self::Result {
        let current_node = self.current_node;

        if segments.len() == 1 {
            // Consider it might be a local variable.
            let segment_name = ast.ident_name(segments[0]);
            for depth in self.get_stack().iter().rev() {
                if let Some((ref_id, import_id)) = depth.get(&segment_name) {
                    println!(
                        "Correctly referenced {ref_id} from {current_node} ['{segment_name}']",
                    );
                    self.component_storage.insert(
                        current_node,
                        TargetReference::new(import_id.clone(), *ref_id),
                    );
                    return self.default_result();
                }
            }
            panic!("Could not find {segment_name} on the stack.");
        }

        let (_, last_id, last_module_id) =
            self.traverse_path(ast, &segments.to_vec(), node_loc.clone())?;

        self.component_storage.insert(
            current_node,
            TargetReference::new(Some(last_module_id), last_id),
        );

        self.default_result()
    }
}
