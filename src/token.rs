/// A single parsed token.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    Eq,
    Lt,
    Le,
    EqEq,
    Ne,
    Ge,
    Gt,
    AndAnd,
    OrOr,
    Not,
    Tilde,

    // Binary Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    And,
    Or,
    Shl,
    Shr,

    // Assignment Binary Operators
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    PercentEq,
    CaretEq,
    AndEq,
    OrEq,
    ShlEq,
    ShrEq,

    // Other symbols
    At,
    Dot,
    DotDot,
    DotDotDot,
    Comma,
    Semi,
    Colon,
    ModSep,
    RArrow,
    LArrow,
    FatArrow,
    Pound,
    Dollar,
    Question,

    // Delimiters
    LParen,
    LBracket,
    LBrace,
    RParen,
    RBracket,
    RBrace,

    // Doc comments
    InnerDoc(String),
    OuterDoc(String),

    // Literals
    Lit(Lit),

    // Lifetime
    Lifetime(String),

    // Identifier-likes
    Ident(String),
    Underscore,

    // Keywords
    Abstract,
    Alignof,
    As,
    Become,
    Box,
    Break,
    Const,
    Continue,
    Crate,
    Do,
    Else,
    Enum,
    Extern,
    Final,
    Fn,
    For,
    If,
    Impl,
    In,
    Let,
    Loop,
    Macro,
    Match,
    Mod,
    Move,
    Mut,
    Offsetof,
    Override,
    Priv,
    Proc,
    Pub,
    Pure,
    Ref,
    Return,
    CapSelf,
    LowSelf,
    Sizeof,
    Static,
    Struct,
    Super,
    Trait,
    Type,
    Typeof,
    Unsafe,
    Unsized,
    Use,
    Virtual,
    Where,
    While,
    Yield,
}

/// A rust literal value.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Lit {
    Byte(u8),
    Char(char),
    Integer(u64, IntSuffix),
    Float(String, FloatSuffix),
    Str(String),
    ByteStr(Vec<u8>),
    Bool(bool),
}

/// The value of the suffix on a floating point number (e.g. `10.4f32` would
/// have the suffix `FloatSuffix::F32`).
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FloatSuffix {
    Unsuffixed,
    F32,
    F64,
}

/// The value of the suffix on a integer number (e.g. `10i32` would have the
/// suffix `IntSuffix::I32`).
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum IntSuffix {
    Unsuffixed,
    Isize,
    Usize,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
}
