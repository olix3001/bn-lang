#![feature(try_trait_v2)]

use crate::{
    ast::{Ast, NodeId},
    error::{AriadneCache, CompilationError, report_parser_errors},
    lexer::{Loc, Token},
    parser::{AstContext, module_parser},
    passes::{Visitor, item_name_binding::ItemNameBindingPass, name_binding::NameBindingPass},
    targets::{Target, target_js::JsTarget},
    utils::{ComponentStorage, pretty_print_ast},
};
use chumsky::{
    Parser,
    input::{Input, Stream},
};
use logos::Logos;

pub(crate) mod ast;
pub(crate) mod error;
pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod utils;

pub(crate) mod passes;
pub(crate) mod targets;

fn main() {
    const EXAMPLE_SOURCE: &'static str = r#"
    // This tells the compiler to just assume that
    // variable 'console' exists globally.
    #[external] const console;

    module math {
      export fn add(a, b) => a + b
      export fn sub(a, b) => a - b
    }

    module utils {
      export const GREET_PREFIX = "Hello, ";
      export const GREET_SUFFIX = "!";
      // Standard methods with fn name() { } are supported too!
      // But this is a shorthand for fn name() { return expr; }
      export fn combine(text, prefix, suffix) => prefix + text + suffix
    }

    // Some examples:
    import math::{add, sub};
    let a = add(sub(3, 1), 2); // a = 4

    console.log(utils::combine(
      "User",
      prefix: utils::GREET_PREFIX,
      suffix: utils::GREET_SUFFIX,
    ));
    "#;

    let token_iter = Token::lexer(EXAMPLE_SOURCE)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(tok) => (tok, Loc::new(0, span)),
            Err(()) => (Token::Error, Loc::new(0, span)),
        });

    let token_stream = Stream::from_iter(token_iter)
        .map(Loc::new(0, 0..EXAMPLE_SOURCE.len()), |(t, s): (_, _)| {
            (t, s)
        });

    let cx = AstContext::new(0);

    let mut cache = AriadneCache::new();
    cache.add_source(0, "test_file".to_string(), EXAMPLE_SOURCE.to_owned());

    let parse_result = module_parser(&cx).parse(token_stream);
    let root = match parse_result.into_result() {
        Ok(result) => {
            pretty_print_ast(&cx.ast.borrow(), result, 0);
            result
        }
        Err(errs) => {
            report_parser_errors(&cache, errs);
            panic!("Panicking due to parser errors.")
        }
    };

    let mut component_storage = ComponentStorage::new();
    let ast = &cx.ast.borrow();

    match compiler_passes(&mut component_storage, ast, root) {
        Ok(()) => {
            let mut target = JsTarget { minify: false };
            println!("\n\n\nTarget output:");
            match target.build((), ast, &component_storage, root) {
                Ok(out) => println!("{out}"),
                Err(err) => err.report(&cache, 0),
            }
        }
        Err(err) => err.report(&cache, 0),
    }
}

fn compiler_passes(
    component_storage: &mut ComponentStorage,
    ast: &Ast,
    root: NodeId,
) -> Result<(), CompilationError> {
    ItemNameBindingPass::new(component_storage).traverse(ast, root)?;
    NameBindingPass::new(component_storage).traverse(ast, root)?;
    Ok(())
}
