use std::any::{Any, TypeId};

use colored::Colorize;
use rustc_hash::FxHashMap;
use slotmap::{SecondaryMap, SlotMap};

use crate::ast::{Ast, ImportGroupItem, ImportTree, ImportTreeKind, ItemName, NodeId, NodeKind};

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

    #[allow(dead_code)]
    pub fn fetch_mut<T: 'static>(&mut self, node: NodeId) -> Option<&mut T> {
        if let Some(cid) = self.id_map.get(&node) {
            if let Some(component) = self.components.get_mut(&TypeId::of::<T>()) {
                return component
                    .get_mut(*cid)
                    .map(|inner| inner.downcast_mut::<T>())
                    .flatten();
            }
        }
        return None;
    }

    #[allow(dead_code)]
    pub fn has<T: 'static>(&self, node: NodeId) -> bool {
        if let Some(cid) = self.id_map.get(&node) {
            if let Some(component) = self.components.get(&TypeId::of::<T>()) {
                return component.contains_key(*cid);
            }
        }
        return false;
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

        NodeKind::Export { stmt, name } => {
            println!(
                "{indent}{} {}:",
                "export as".yellow(),
                ast.get_ident_name(*name).unwrap_or("<self>").cyan()
            );
            pretty_print_ast(ast, *stmt, ind + 1);
        }

        NodeKind::ReturnStmt { value, .. } => match value {
            Some(value) => println!(
                "{indent}{} {}",
                "return".yellow(),
                pretty_print_expr(ast, *value)
            ),
            None => println!("{indent}{} {}", "return".yellow(), "void".bright_purple()),
        },

        NodeKind::Block {
            stmts,
            trailing_expr,
        } => {
            println!("{indent}{} {{", "block".yellow());
            for stmt in stmts.iter() {
                pretty_print_ast(ast, *stmt, ind + 1);
            }
            if let Some(trailing) = trailing_expr {
                println!(
                    "{}{}: {}",
                    " ".repeat((ind + 1) * 4),
                    "trailing".yellow(),
                    pretty_print_expr(ast, *trailing)
                );
            }
            println!("{indent}}}");
        }

        NodeKind::FunctionDef(def) => {
            let name = match &def.name {
                ItemName::Identifier(name) => ast.ident_name(*name).cyan(),
                _ => "<todo:computed>".red(),
            };
            let params = def
                .params
                .iter()
                .map(|param| match &param.default_value {
                    Some(default_value) => format!(
                        "{}: {}",
                        ast.ident_name(param.name).cyan(),
                        pretty_print_expr(ast, *default_value)
                    )
                    .to_string(),
                    None => ast.ident_name(param.name).cyan().to_string(),
                })
                .collect::<Vec<_>>()
                .join(", ");

            println!("{indent}{} {}({}):", "function".yellow(), name, params);
            pretty_print_ast(ast, def.body, ind + 1);
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

        NodeKind::ExprStmt(expr) => {
            println!("{indent}{}", pretty_print_expr(ast, *expr));
        }

        _ => println!("{indent}{}", "<unknown statement>".red()),
    }
}

fn pretty_print_expr(ast: &Ast, expr: NodeId) -> String {
    if expr.0 == usize::MAX {
        return format!("{}", "<dummy>".black().on_bright_yellow());
    }
    let cnode = &ast.nodes[expr.0];

    match &cnode.kind {
        NodeKind::NumberLit(value) => value.bright_green().to_string(),
        NodeKind::BooleanLit(value) => value.to_string().bright_green().to_string(),
        NodeKind::StringLit(value) => format!("{value:?}").bright_green().to_string(),
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

        NodeKind::Call { callee, args } => {
            format!(
                "{}({})",
                pretty_print_expr(ast, *callee),
                args.iter()
                    .map(|arg| {
                        match &arg.name {
                            Some(name) => format!(
                                "{}: {}",
                                ast.ident_name(*name).cyan(),
                                pretty_print_expr(ast, arg.value)
                            ),
                            None => pretty_print_expr(ast, arg.value),
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }

        NodeKind::MemberAccess {
            object,
            member,
            computed,
        } => match computed {
            true => format!(
                "{}[{}]",
                pretty_print_expr(ast, *object),
                pretty_print_expr(ast, *member)
            ),
            false => format!(
                "{}.{}",
                pretty_print_expr(ast, *object),
                ast.ident_name(*member).bright_green()
            ),
        },

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
