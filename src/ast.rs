use crate::lexer::{Loc, SourceId, Token as LexerToken}; // For raw attribute tokens
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize); // Made field public for easier access in parser

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NodeId({})", self.0)
    }
}

#[derive(Debug)]
pub struct Ast {
    pub nodes: Vec<Node>,
    pub source_id: SourceId,
    // Potentially pools for strings, identifiers, etc., to reduce duplication if needed.
    // For now, strings are stored directly in NodeKind.
}

impl Ast {
    pub fn new(source_id: SourceId) -> Self {
        Self {
            nodes: Vec::new(),
            source_id,
        }
    }

    pub fn add_node(&mut self, kind: NodeKind, loc: Loc) -> NodeId {
        let id = NodeId(self.nodes.len());
        self.nodes.push(Node { id, kind, loc });
        id
    }

    #[allow(dead_code)]
    pub fn get_node(&self, id: NodeId) -> Option<&Node> {
        self.nodes.get(id.0)
    }

    #[allow(dead_code)]
    pub fn get_node_mut(&mut self, id: NodeId) -> Option<&mut Node> {
        self.nodes.get_mut(id.0)
    }

    // Example interaction methods:
    #[allow(dead_code)]
    pub fn get_ident_name(&self, id: Option<NodeId>) -> Option<&str> {
        let Some(id) = id else {
            return None;
        };
        self.get_node(id).and_then(|node| {
            if let NodeKind::Identifier(name) = &node.kind {
                Some(name.as_str())
            } else {
                None
            }
        })
    }

    #[allow(dead_code)]
    pub fn ident_name(&self, id: NodeId) -> String {
        self.get_ident_name(Some(id)).unwrap().to_owned()
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    pub id: NodeId,
    pub kind: NodeKind,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    // Module
    Module(Module),

    // Literals
    NumberLit(String),
    StringLit(String),
    TemplateLit {
        parts: Vec<TemplatePart>,
    },
    BooleanLit(bool),
    NullLit,
    UndefinedLit,
    ArrayLit(Vec<NodeId>),
    StructLit {
        name: Option<NodeId>,
        fields: Vec<(StructLitFieldKey, NodeId)>,
        loc: Loc,
    }, // Unnamed struct #{ key: value }

    // Identifiers & Paths
    Identifier(String), // Raw string for identifier
    Path {
        segments: Vec<NodeId>,
    }, // Vec of Identifier NodeIds

    // Expressions
    BinaryOp {
        op: BinaryOp,
        left: NodeId,
        right: NodeId,
    },
    UnaryOp {
        op: UnaryOp,
        operand: NodeId,
    },
    Call {
        callee: NodeId,
        args: Vec<Arg>,
        tail_closure: Option<NodeId>,
    }, // tail_closure is NodeId of a ClosureExpr
    MemberAccess {
        object: NodeId,
        member: NodeId, /* Identifier or Expression (for computed) */
        computed: bool,
    },
    Await {
        expr: NodeId,
    },
    Block {
        stmts: Vec<NodeId>,            // Statements or declarations
        trailing_expr: Option<NodeId>, // If the block ends with an expression for implicit return
    },
    Export {
        stmt: NodeId,
        name: Option<NodeId>,
    },
    IfExpr {
        cond: NodeId,
        then_branch: NodeId, /* Block or Expr */
        else_branch: Option<NodeId /* Block or Expr or IfExpr */>,
    },
    LoopExpr {
        label: Option<NodeId /* Identifier */>,
        block: NodeId, /* Block */
    },
    WhileExpr {
        label: Option<NodeId /* Identifier */>,
        cond: NodeId,
        block: NodeId, /* Block */
    },
    ForExpr {
        label: Option<NodeId /* Identifier */>,
        var_name: NodeId, /* Identifier */
        iterator: NodeId,
        block: NodeId, /* Block */
    },
    MatchExpr {
        expr: NodeId,
        arms: Vec<MatchArm>,
    },
    ClosureExpr {
        params: Vec<Param>,
        body: NodeId, /* Block or Expr */
        loc: Loc,
    },

    // Statements (can also be expressions in some contexts)
    LetDecl {
        attributes: Vec<Attribute>,
        name: NodeId, /* Identifier */
        value: Option<NodeId>,
    }, // value is optional
    ConstDecl {
        attributes: Vec<Attribute>,
        name: NodeId, /* Identifier */
        value: NodeId,
    },
    ExprStmt(NodeId), // An expression used as a statement
    ReturnStmt {
        attributes: Vec<Attribute>,
        value: Option<NodeId>,
    },
    YieldStmt {
        attributes: Vec<Attribute>,
        value: Option<NodeId>,
    },
    BreakStmt {
        attributes: Vec<Attribute>,
        label: Option<NodeId /* Identifier */>,
    },
    ContinueStmt {
        attributes: Vec<Attribute>,
        label: Option<NodeId /* Identifier */>,
    },

    // Item definitions
    FunctionDef(Function),
    StructDef(Struct),
    EnumDef(Enum),
    ImportDef(Import),

    // Other specific nodes
    LabelNode {
        name: NodeId, /* Identifier */
        item: NodeId, /* The item being labelled */
    },

    Error, // Parsing failed here.
}

#[derive(Debug, Clone)]
pub enum TemplatePart {
    Literal(String),    // Raw string part
    Expression(NodeId), // NodeId of the expression inside {expr}
}

#[derive(Debug, Clone)]
pub enum StructLitFieldKey {
    Identifier(NodeId), // Identifier NodeId
    Expression(NodeId), // Computed key, e.g. [expr]
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub name: Option<NodeId /* Identifier for named arg */>,
    pub value: NodeId, // Expression NodeId
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub attributes: Vec<Attribute>,
    pub name: NodeId,                  // Identifier NodeId
    pub default_value: Option<NodeId>, // Expression NodeId for default
    // Type annotation could be added here: pub type_ann: Option<NodeId>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub path: Vec<NodeId>, // Vec of Identifier NodeIds, e.g., [ident] or [ident, segment]
    pub args_raw_tokens: Option<Vec<(LexerToken, Loc)>>, // Raw tokens for #[attr(...)]
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum ItemName {
    // For functions, structs, enums that can have computed names
    Identifier(NodeId),
    Computed(NodeId /* Expression */),
}

#[derive(Debug, Clone)]
pub struct Module {
    pub attributes: Vec<Attribute>,
    pub name: Option<NodeId>,
    pub stmts: Vec<NodeId>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub attributes: Vec<Attribute>,
    pub name: ItemName,
    pub params: Vec<Param>,
    pub body: NodeId, // NodeId of a Block or an Expression
    pub is_async: bool,
    pub is_generator: bool,
    pub loc: Loc,
    // pub return_type_ann: Option<NodeId>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub attributes: Vec<Attribute>,
    pub name: ItemName, // Typically Identifier, but spec allows computed names for members
    pub members: Vec<StructMember>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum StructMember {
    Field {
        attributes: Vec<Attribute>,
        name: ItemName, // Identifier or Computed Expression
        // pub type_ann: Option<NodeId>,
        // pub default_value: Option<NodeId>,
        loc: Loc,
    },
    Method(Function), // Re-use Function struct (which has its own NodeId if it's a separate definition)
                      // Or store Function data directly. If methods are distinct items, Method(NodeId) might be better.
                      // For simplicity, embedding Function data here.
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub attributes: Vec<Attribute>,
    pub name: ItemName,
    pub variants: Vec<EnumVariant>,
    pub methods: Vec<Function>, // Similar to struct methods
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub attributes: Vec<Attribute>,
    pub name: NodeId, // Identifier NodeId
    pub kind: EnumVariantKind,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum EnumVariantKind {
    Simple, // VariantA
    TupleStyle(
        Vec<
            NodeId, /* Type or Arity Placeholder. For now, Expression NodeId for syntax like VariantB[expr, expr] */
        >,
    ), // VariantB[expr1, expr2]
    StructStyle(Vec<Param /* { name: Type, ... } like params */>), // VariantC { a, b, c }
}

#[derive(Debug, Clone)]
pub struct Import {
    pub attributes: Vec<Attribute>,
    pub tree: ImportTree,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ImportTree {
    pub prefix: Vec<NodeId /* Identifier */>, // e.g., [a, b] for `a::b::{...}` or `a::b::c`
    pub kind: ImportTreeKind,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum ImportTreeKind {
    Leaf, // Represents the end of a path, like `import a::b::c;` where `c` is the final part of `prefix`.
    Group(Vec<ImportGroupItem>), // Represents `{ ... }`
}

#[derive(Debug, Clone)]
pub enum ImportGroupItem {
    Path(ImportTree), // c::d (becomes its own ImportTree)
    SelfImport(Loc),  // self
    RenamedPath {
        tree: ImportTree,
        new_name: NodeId, /* Identifier */
        loc: Loc,
    }, // item as new_name
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Comparison
    EqEq,
    NotEq,
    AboutEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    // Logical
    And,
    Or,
    // Binary
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
    // Assignment
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    // Range
    Range,
    RangeInclusive,
    // Others could be added: Bitwise, etc.
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Mod => write!(f, "%"),

            BinaryOp::EqEq => write!(f, "=="),
            BinaryOp::AboutEq => write!(f, "~="),
            BinaryOp::Greater => write!(f, ">"),
            BinaryOp::GreaterEq => write!(f, ">="),
            BinaryOp::Less => write!(f, "<"),
            BinaryOp::LessEq => write!(f, "<="),
            BinaryOp::NotEq => write!(f, "!="),

            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or => write!(f, "or"),

            // ... other ops
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,     // -
    Not,     // !
    PreInc,  // ++i
    PreDec,  // --i
    PostInc, // i++
    PostDec, // i--
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::PreInc => write!(f, "++#"),
            UnaryOp::PreDec => write!(f, "--#"),
            UnaryOp::PostInc => write!(f, "#++"),
            UnaryOp::PostDec => write!(f, "#--"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: NodeId,       // Pattern NodeId
    pub guard: Option<NodeId>, // If condition Expression NodeId
    pub body: NodeId,          // Expression or Block NodeId
    pub loc: Loc,
}

// Pattern AST (simplified for now, can be expanded)
// For now, MatchArm.pattern can be an expression ID that the compiler interprets as a pattern.
// A full pattern system would have its own NodeKind variants.
