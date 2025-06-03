use std::{
    any::{Any, TypeId},
    collections::hash_map::Entry,
};

use colored::Colorize;
use rustc_hash::FxHashMap;
use slotmap::{SecondaryMap, SlotMap};

use crate::ast::{Ast, ImportGroupItem, ImportTree, ImportTreeKind, NodeId, NodeKind};

slotmap::new_key_type! {
    struct ComponentId;
}

/// Storage for components, so we can easily check references, callability and other stuff on the AST.
#[derive(Default)]
pub struct ComponentStorage {
    id_map: FxHashMap<NodeId, ComponentId>,
    id_storage: SlotMap<ComponentId, ()>,
    components: FxHashMap<TypeId, SecondaryMap<ComponentId, Box<dyn Any>>>,
}

impl ComponentStorage {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert<T: 'static>(&mut self, node: NodeId, component: T) {
        let cid = *self
            .id_map
            .entry(node)
            .or_insert_with(|| self.id_storage.insert(()));

        let component_map = self
            .components
            .entry(TypeId::of::<T>())
            .or_insert_with(|| SecondaryMap::new());
        component_map.insert(cid, Box::new(component));
    }

    pub fn fetch<T: 'static>(&self, node: NodeId) -> Option<&T> {
        if let Some(cid) = self.id_map.get(&node) {
            if let Some(component) = self.components.get(&TypeId::of::<T>()) {
                return component
                    .get(*cid)
                    .map(|inner| inner.downcast_ref::<T>())
                    .flatten();
            }
        }
        return None;
    }
}

pub fn pretty_print_ast(ast: &Ast, root: NodeId, ind: usize) {
    let cnode = &ast.nodes[root.0];
    let indent = " ".repeat(ind * 4);

    match &cnode.kind {
        NodeKind::Module(module) => {
            println!(
                "{indent}{} {} {{",
                "module".yellow(),
                ast.get_ident_name(module.name)
                    .unwrap_or("<unnamed>")
                    .cyan()
            );
            for stmt in module.stmts.iter() {
                pretty_print_ast(ast, *stmt, ind + 1);
            }
            println!("{indent}}}")
        }

        NodeKind::ImportDef(import) => {
            println!(
                "{indent}{} {}",
                "import".yellow(),
                pretty_import_tree(ast, &import.tree)
            )
        }

        NodeKind::ConstDecl { name, value, .. } => {
            println!(
                "{indent}{} {} = {}",
                "const".yellow(),
                ast.get_ident_name(Some(*name)).unwrap().cyan(),
                pretty_print_expr(ast, *value)
            )
        }
        NodeKind::LetDecl { name, value, .. } => {
            if let Some(value) = value {
                println!(
                    "{indent}{} {} = {}",
                    "let".yellow(),
                    ast.get_ident_name(Some(*name)).unwrap().cyan(),
                    pretty_print_expr(ast, *value)
                )
            } else {
                println!(
                    "{indent}{} {}",
                    "let".yellow(),
                    ast.get_ident_name(Some(*name)).unwrap().cyan()
                )
            }
        }

        _ => println!("{indent}{}", "<unknown statement>".red()),
    }
}

fn pretty_print_expr(ast: &Ast, expr: NodeId) -> String {
    let cnode = &ast.nodes[expr.0];

    match &cnode.kind {
        NodeKind::NumberLit(value) => value.bright_green().to_string(),
        NodeKind::Path { segments } => pretty_path(ast, segments),

        NodeKind::BinaryOp { op, left, right } => {
            format!(
                "({} {} {})",
                pretty_print_expr(ast, *left),
                op.to_string().bright_purple(),
                pretty_print_expr(ast, *right)
            )
        }
        NodeKind::UnaryOp { op, operand } => {
            format!(
                "({} {})",
                op.to_string().bright_purple(),
                pretty_print_expr(ast, *operand)
            )
        }

        _ => "<unknown expression>".red().to_string(),
    }
}

fn pretty_path(ast: &Ast, path: &Vec<NodeId>) -> String {
    path.iter()
        .map(|ident| ast.get_ident_name(Some(*ident)).unwrap())
        .collect::<Vec<_>>()
        .join("::")
}

fn pretty_import_tree(ast: &Ast, tree: &ImportTree) -> String {
    let mut result = format!("{}", pretty_path(ast, &tree.prefix));
    match &tree.kind {
        ImportTreeKind::Leaf => {}
        ImportTreeKind::Group(group) => {
            result += &format!(
                "::{{{}}}",
                group
                    .iter()
                    .map(|item| {
                        match item {
                            ImportGroupItem::Path(subtree) => pretty_import_tree(ast, subtree),
                            ImportGroupItem::SelfImport(..) => "self".yellow().to_string(),
                            ImportGroupItem::RenamedPath { tree, new_name, .. } => {
                                format!(
                                    "{} {} {}",
                                    pretty_import_tree(ast, tree),
                                    "as".yellow(),
                                    ast.get_ident_name(Some(*new_name)).unwrap().cyan()
                                )
                            }
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }
    result
}
