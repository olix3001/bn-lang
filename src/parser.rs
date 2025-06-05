use std::{cell::RefCell, rc::Rc};

use chumsky::{Parser, extra, input::ValueInput, prelude::*};
use logos::Source;

use crate::{
    ast::{
        self, Ast, BinaryOp, ImportGroupItem, ImportTreeKind, ItemName, NodeId, NodeKind, Param,
        UnaryOp,
    },
    error::RichParseError,
    lexer::{Loc, SourceId, Token},
};

impl chumsky::span::Span for Loc {
    type Context = SourceId;
    type Offset = usize;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Loc::new(context, range)
    }

    fn context(&self) -> Self::Context {
        self.source
    }
    fn start(&self) -> Self::Offset {
        self.span.start
    }
    fn end(&self) -> Self::Offset {
        self.span.end
    }
}

// Context used for building the flat AST during parsing.
#[derive(Clone)]
pub struct AstContext {
    pub ast: Rc<RefCell<Ast>>,
    source_id: SourceId,
}

impl AstContext {
    pub fn new(source_id: SourceId) -> Self {
        Self {
            ast: Rc::new(RefCell::new(Ast::new(source_id))),
            source_id,
        }
    }

    pub fn add_node(&self, kind: NodeKind, loc: Loc) -> NodeId {
        self.ast.borrow_mut().add_node(kind, loc)
    }

    #[allow(dead_code)]
    pub fn to_loc(&self, span: SimpleSpan) -> Loc {
        Loc::new(self.source_id, span.into_range())
    }
}

type ParserContext<'a> = &'a AstContext;
type ParserExtra<'a> = extra::Err<RichParseError<'a>>;

macro_rules! parser_functions {
    ($(
        $vis:vis fn $name:ident($cx:ident $(,$arg:ident: $typ:ty)*) -> $out:ty => $code:tt
    )+) => {
        $(
            $vis fn $name<'src, I>($cx: ParserContext<'src> $(, $arg: $typ)*)
                -> impl Parser<'src, I, $out, ParserExtra<'src>> + Clone
                    where I: ValueInput<'src, Token = Token, Span = Loc>, $code
        )*
    };
}

// ---< HELPER PARSERS >---
parser_functions! {
    fn ident_parser(cx) -> NodeId => {
        select! { Token::Identifier(ident) => NodeKind::Identifier(ident) }
            .map_with(|tok, e| cx.add_node(tok, e.span()))
            .labelled("identifier")
    }

    fn attribute_parser(cx, module_level: bool) -> Vec<ast::Attribute> => {
        let path = path_parser(cx);
        let nl = just(Token::NL).or_not().ignored();
        let raw_token = none_of(Token::RightParen).map_with(|tok: Token, e| (tok, e.span()));

        let beginning = match module_level {
            true => just(Token::Hash).ignore_then(just(Token::Bang)).boxed(),
            false => just(Token::Hash).boxed()
        };
        let single_attribute = beginning
            .ignore_then(just(Token::LeftBracket))
            .ignore_then(path)
            .then(raw_token.repeated().collect().delimited_by(just(Token::LeftParen), just(Token::RightParen)).or_not())
            .map_with(|(path, args_opt), e| ast::Attribute { path, args_raw_tokens: args_opt, loc: e.span() })
            .then_ignore(just(Token::RightBracket))
            .labelled("attribute");

        single_attribute.then_ignore(nl).repeated().collect().boxed()
    }

    fn path_parser(cx) -> Vec<NodeId> => {
        ident_parser(cx)
            .separated_by(just(Token::DoubleColon))
            .at_least(1)
            .collect::<Vec<_>>()
            .labelled("path")
            .boxed()
    }
}

// ---< MAIN PARSERS >---
parser_functions! {
    pub fn statement_parser(cx) -> NodeId => { // TODO: This could take `expr_parse: impl Parser`, which would make this a lot more organised.
        let mut statement_parser_rec = Recursive::declare();
        let ident = ident_parser(cx);
        let expr = expr_parser(cx);

        // ---< THINGS THAT NEED RECURSIVE STATEMENTS >---
        // Module
        let mod_attributes_parser = attribute_parser(cx, true).or_not().map(|v| v.unwrap_or(Vec::new()));
        let module_content_parser = just(Token::NL).repeated().ignore_then(
            mod_attributes_parser.then(statement_parser_rec.clone().repeated().collect()).map_with(|(attrs, stmts), e| {
                cx.add_node(NodeKind::Module(ast::Module {
                    attributes: attrs,
                    name: None,
                    stmts,
                    loc: e.span()
                }), e.span())
            })
        ).then_ignore(just(Token::NL).repeated());
        let inline_module_parser = just(Token::ModuleKW)
            .ignore_then(ident.clone())
            .then(module_content_parser.delimited_by(just(Token::LeftCurly), just(Token::RightCurly)))
            .map(|(name, module_id)| {
                let mut ast = cx.ast.borrow_mut();
                let node_mut = ast.get_node_mut(module_id).unwrap();
                let NodeKind::Module(module) = &mut node_mut.kind
                    else { unreachable!("This parser only module!") };
                module.name = Some(name);
                module_id
            }).labelled("inline module declaration").boxed();

        let export_statement_parser = just(Token::ExportKW)
            .ignore_then(statement_parser_rec.clone())
            .map_with(|stmt, e| cx.add_node(NodeKind::Export {
                stmt,
                name: None // TODO
            }, e.span())).boxed();

        let attributes_parser = attribute_parser(cx, false);

        let code_block_parser = statement_parser_rec.clone()
            .repeated().collect::<Vec<_>>()
            .delimited_by(just(Token::LeftCurly), just(Token::RightCurly))
            .map(|body| {
                NodeKind::Block { stmts: body, trailing_expr: None }
            });

        let function_body_parser = choice((
            just(Token::FatArrow).ignore_then(expr.clone())
                .map(|expr| NodeKind::ReturnStmt { attributes: Vec::new(), value: Some(expr) }),
            code_block_parser.clone()
        ));

        let function_parser = attributes_parser.clone()
            .then(func_modifiers_parser(cx))
            .then_ignore(just(Token::FuncKW))
            .then(ident.clone())
            .then(func_params_parser(cx).delimited_by(just(Token::LeftParen), just(Token::RightParen)))
            .then(function_body_parser.map_with(|body, e| cx.add_node(body, e.span())))
            .map_with(|((((attributes, modifiers), name), params), body), e| cx.add_node(
                NodeKind::FunctionDef(ast::Function {
                    attributes,
                    name: ItemName::Identifier(name), // Computed names are possible in structs/objects only.
                    params,
                    body,
                    is_async: modifiers.0,
                    is_generator: modifiers.1,
                    loc: e.span()
                }),
                e.span()
            )).labelled("function definition").boxed();

        let return_stmt_parser = attributes_parser.clone()
            .then_ignore(just(Token::ReturnKW))
            .then(expr.clone().or_not())
            .map_with(|(attributes, value), e| cx.add_node(NodeKind::ReturnStmt { attributes, value }, e.span()))
            .labelled("return statement").boxed();

        // ---< STATEMENTS >---
        statement_parser_rec.define(
            just(Token::NL).repeated().ignore_then(
                choice((
                    import_parser(cx),
                    inline_module_parser,
                    export_statement_parser,
                    function_parser,
                    const_decl_parser(cx),
                    let_decl_parser(cx),
                    return_stmt_parser,

                    expr.map_with(|node, e| cx.add_node(NodeKind::ExprStmt(node), e.span())),
                )).then_ignore(just(Token::NL).or(just(Token::Semicolon)).repeated()).labelled("statement")
            )
        );
        statement_parser_rec
    }

    pub fn func_modifiers_parser(cx) -> (bool, bool) => {
        just(Token::AsyncKW).or(just(Token::GenKW)).or_not()
            .map(|tok| match tok {
                Some(Token::AsyncKW) => (true, false),
                Some(Token::GenKW) => (false, true),
                None => (false, false),
                _ => unreachable!()
            }).labelled("function modifiers").boxed()
    }

    pub fn func_params_parser(cx) -> Vec<Param> => {
        let ident = ident_parser(cx);
        let attribs = attribute_parser(cx, false);

        let param =
            attribs.then(
                ident
                .then(
                    just(Token::Colon)
                    .ignore_then(expr_parser(cx))
                    .or_not()
            )).map_with(|(attributes, (name, default_value)), e| Param {
                attributes,
                name,
                default_value,
                loc: e.span()
            });

        param.separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .labelled("function parameters")
            .boxed()
    }

    pub fn expr_parser(cx) -> NodeId => {
        use chumsky::pratt::{prefix, infix, postfix, left};

        let ident = ident_parser(cx);
        let mut expr_parser_rec = Recursive::declare();
        let call_arguments_parser = choice((
            ident.clone().then_ignore(just(Token::Colon)).then(expr_parser_rec.clone()).map_with(|(name, value), e| ast::Arg {
                name: Some(name),
                value,
                loc: e.span()
            }),
            expr_parser_rec.clone().map_with(|value, e| ast::Arg {
                name: None,
                value,
                loc: e.span()
            })
        )).separated_by(just(Token::Comma)).collect::<Vec<_>>()
            .labelled("call argument");

        let atom = just(Token::NL).repeated().ignore_then(
            choice((
                literal_expr_parser(cx),
            )).then_ignore(just(Token::NL).or_not())
        ).map_with(|atom, e| (atom, e.span()));

        // Fold with left-associativity
        fn la_fold(cx: ParserContext, op: BinaryOp, l: (NodeId, Loc), r: (NodeId, Loc)) -> (NodeId, Loc) {
            let new_span = Loc::new(cx.source_id, l.1.span.start..r.1.span.end);
            (cx.add_node(
                NodeKind::BinaryOp { op, left: l.0, right: r.0 },
                new_span.clone()
            ), new_span)
        }

        // Fold with unary prefix.
        fn unary_fold(cx: ParserContext, op: UnaryOp, operand: (NodeId, Loc)) -> (NodeId, Loc) {
            (cx.add_node(
                NodeKind::UnaryOp { op, operand: operand.0 },
                operand.1.clone()
            ), operand.1)
        }

        let call_operator = call_arguments_parser
            .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            .boxed();

        let static_member_access = just(Token::Dot).ignore_then(ident);
        let computed_member_access = expr_parser_rec.clone().delimited_by(just(Token::LeftBracket), just(Token::RightBracket));

        let pratt_expr = atom.boxed().pratt((
            // This seems weird but it is actually a great idea.
            postfix(10, static_member_access, |lhs: (NodeId, Loc), member, _| {
                (cx.add_node(
                    NodeKind::MemberAccess { object: lhs.0, member: member, computed: false },
                    lhs.1.clone()
                ), lhs.1)
            }),
            postfix(10, computed_member_access, |lhs: (NodeId, Loc), member, _| {
                (cx.add_node(
                    NodeKind::MemberAccess { object: lhs.0, member: member, computed: true },
                    lhs.1.clone()
                ), lhs.1)
            }),
            postfix(10, call_operator, |lhs: (NodeId, Loc), args, _| {
               (cx.add_node(
                   NodeKind::Call { callee: lhs.0, args },
                   lhs.1.clone()
               ), lhs.1)
            }),

            // Regular operators.
            postfix(8, just(Token::Increment), |lhs, _, _| unary_fold(cx, UnaryOp::PostInc, lhs)),
            postfix(8, just(Token::Decrement), |lhs, _, _| unary_fold(cx, UnaryOp::PostDec, lhs)),

            prefix(7, just(Token::Increment), |_, rhs, _| unary_fold(cx, UnaryOp::PreInc, rhs)),
            prefix(7, just(Token::Decrement), |_, rhs, _| unary_fold(cx, UnaryOp::PreDec, rhs)),
            prefix(7, just(Token::Bang), |_, rhs, _| unary_fold(cx, UnaryOp::Not, rhs)),
            prefix(7, just(Token::Minus), |_, rhs, _| unary_fold(cx, UnaryOp::Neg, rhs)),

            infix(left(6), just(Token::Star), |l, _, r, _| la_fold(cx, BinaryOp::Mul, l, r)),
            infix(left(6), just(Token::Slash), |l, _, r, _| la_fold(cx, BinaryOp::Div, l, r)),

            infix(left(5), just(Token::Percent), |l, _, r, _| la_fold(cx, BinaryOp::Mod, l, r)),

            infix(left(4), just(Token::Plus), |l, _, r, _| la_fold(cx, BinaryOp::Add, l, r)),
            infix(left(4), just(Token::Minus), |l, _, r, _| la_fold(cx, BinaryOp::Sub, l, r)),

            infix(left(3), just(Token::LessEqual), |l, _, r, _| la_fold(cx, BinaryOp::LessEq, l, r)),
            infix(left(3), just(Token::GreaterEqual), |l, _, r, _| la_fold(cx, BinaryOp::GreaterEq, l, r)),
            infix(left(3), just(Token::LeftAngle), |l, _, r, _| la_fold(cx, BinaryOp::Less, l, r)),
            infix(left(3), just(Token::RightAngle), |l, _, r, _| la_fold(cx, BinaryOp::Greater, l, r)),

            infix(left(2), just(Token::AboutEqual), |l, _, r, _| la_fold(cx, BinaryOp::AboutEq, l, r)),
            infix(left(2), just(Token::EqualEqual), |l, _, r, _| la_fold(cx, BinaryOp::EqEq, l, r)),
            infix(left(2), just(Token::NotEqual), |l, _, r, _| la_fold(cx, BinaryOp::NotEq, l, r)),

            infix(left(1), just(Token::Conjunction), |l, _, r, _| la_fold(cx, BinaryOp::And, l, r)),
            infix(left(1), just(Token::Disjunction), |l, _, r, _| la_fold(cx, BinaryOp::Or, l, r)),
        ));

        let final_parser = pratt_expr.map(|val| val.0).labelled("expression").boxed();
        expr_parser_rec.define(choice((
            final_parser.clone().delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            final_parser
        )));
        expr_parser_rec
    }

    pub fn module_parser(cx) -> NodeId => {
        let mod_attributes = attribute_parser(cx, true).or_not().map(|v| v.unwrap_or(Vec::new()));

        just(Token::NL).repeated().ignore_then(
            mod_attributes.then(statement_parser(cx).repeated().collect()).map_with(|(attrs, stmts), e| {
                cx.add_node(NodeKind::Module(ast::Module {
                    attributes: attrs,
                    name: None,
                    stmts,
                    loc: e.span()
                }), e.span())
            })
        ).then_ignore(just(Token::NL).repeated())
    }

    // ---< STATEMENTS >---
    fn const_decl_parser(cx) -> NodeId => {
        let ident = ident_parser(cx);

        attribute_parser(cx, false)
            .then_ignore(just(Token::ConstKW))
            .then(ident)
            .then(
                just(Token::Assign)
                    .ignore_then(expr_parser(cx))
                    .or_not()
            )
            .map_with(|((attributes, name), value), e| cx.add_node(
                NodeKind::ConstDecl { attributes, name, value: value.unwrap_or(NodeId(usize::MAX)) },
                e.span()
            )).labelled("const variable declaration").boxed()
    }

    fn let_decl_parser(cx) -> NodeId => {
        let ident = ident_parser(cx);

        attribute_parser(cx, false)
            .then_ignore(just(Token::LetKW))
            .then(ident)
            .then(
                just(Token::Assign)
                    .ignore_then(expr_parser(cx))
                    .or_not()
            )
            .map_with(|((attributes, name), value), e| cx.add_node(
                NodeKind::LetDecl { attributes, name, value },
                e.span()
            )).labelled("let variable declaration").boxed()
    }

    fn import_parser(cx) -> NodeId => {
        let ident = ident_parser(cx);
        let path = path_parser(cx);

        let mut import_tree_parser = Recursive::declare();
        let mut group_item_parser = Recursive::declare();

        let renamed_path_item = import_tree_parser.clone()
            .then_ignore(just(Token::AsKW))
            .then(ident.clone())
            .map_with(|(tree, new_name), e| ImportGroupItem::RenamedPath {
                tree, new_name, loc: e.span()
            });

        let self_item = just(Token::SelfKW)
            .map_with(|_, e| ImportGroupItem::SelfImport(e.span()));

        let path_item = import_tree_parser.clone()
            .map(ImportGroupItem::Path);

        group_item_parser.define(
            choice((
                renamed_path_item,
                self_item,
                path_item
            )).labelled("import group item")
        );

        let group_items = group_item_parser
            .then_ignore(just(Token::NL).or_not())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>();

        let group_kind = group_items
            .delimited_by(just(Token::LeftCurly), just(Token::RightCurly))
            .map(ImportTreeKind::Group)
            .labelled("import group");

        import_tree_parser.define(
            path.then(
                just(Token::DoubleColon)
                    .ignore_then(group_kind.clone())
                    .or_not()
            ).map_with(|(prefix, opt_group_kind), e| {
                if let Some(group_kind_val) = opt_group_kind {
                    ast::ImportTree {
                        prefix,
                        kind: group_kind_val,
                        loc: e.span()
                    }
                } else {
                    ast::ImportTree {
                        prefix,
                        kind: ImportTreeKind::Leaf,
                        loc: e.span()
                    }
                }
            })
            .labelled("import tree")
        );

        attribute_parser(cx, false)
            .then_ignore(just(Token::ImportKW))
            .then(import_tree_parser)
            .map_with(|(attributes, tree), e| cx.add_node(
                NodeKind::ImportDef(
                    ast::Import {
                        attributes,
                        tree,
                        loc: e.span()
                    }
                ), e.span()))
            .then_ignore(just(Token::Semicolon))
            .labelled("import statement")
            .boxed()
    }

    // ---< EXPRESSIONS >---
    fn literal_expr_parser(cx) -> NodeId => {
        let number_literal = select! {
            Token::NumberLiteral(value) => NodeKind::NumberLit(value)
        };
        let string_literal = select! {
            Token::StringLiteral(value) => NodeKind::StringLit(
                unescape::unescape(
                    value.slice(1..value.len()-1).unwrap()
                ).unwrap().to_string()
            )
        };
        let boolean_literal = select! {
            Token::TrueKW => NodeKind::BooleanLit(true),
            Token::FalseKW => NodeKind::BooleanLit(false),
        };

        choice((
            number_literal,
            string_literal,
            boolean_literal,
            path_parser(cx).map(|path| NodeKind::Path { segments: path })
        )).map_with(|node, e| cx.add_node(node, e.span()))
    }
}
