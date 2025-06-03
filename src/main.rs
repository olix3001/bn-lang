#![feature(type_alias_impl_trait)]

use crate::{
    error::{AriadneCache, report_parser_errors},
    lexer::{Loc, Token},
    parser::{AstContext, module_parser},
    utils::pretty_print_ast,
};
use chumsky::{
    Parser,
    input::{Input, Stream},
};
use logos::Logos;

pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod utils;

fn main() {
    const EXAMPLE_SOURCE: &'static str = "
    #![bruh]
    #[hello(world)] import hello::world;
    import lorem::ipsum::{dolor::{self, sit}, amet::something};

    const hello = 1;
    let world = 2 * 2 - 2 == 8 || 4 < 5;
    let lorem;
    ";

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

    let parse_result = module_parser(&cx).parse(token_stream);
    match parse_result.into_result() {
        Ok(result) => {
            pretty_print_ast(&cx.ast.borrow(), result, 0);
        }
        Err(errs) => {
            let mut cache = AriadneCache::new();
            cache.add_source(0, "test_file".to_string(), EXAMPLE_SOURCE.to_owned());
            report_parser_errors(&cache, errs);
        }
    }
}
