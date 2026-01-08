#![allow(dead_code, unused_imports, unused_variables)]

// Token definitions for the Lingua lexer
#[derive(Debug, PartialEq)]
pub enum Token {
    // Keywords
    Fn,
    Let,
    Mut,
    If,
    Else,
    Loop,
    While,
    Return,
    Use,
    Pub,
    Struct,
    Enum,
    Impl,
    Trait,
    Type,
    Const,
    Static,
    Match,
    True,
    False,

    // Types
    I32,
    I64,
    F32,
    F64,
    Bool,

    // Literals
    Int(i32),
    Float(f64),
    String(String),

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Eq,
    NotEq,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Not,
    AndAnd,
    OrOr,
    Caret,
    Shl,
    Shr,

    // Punctuation
    Arrow,
    FatArrow,
    Colon,
    Semicolon,
    Comma,
    Dot,
    DotDot,
    ColonColon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Identifiers
    Ident(String),

    // EOF
    Eof,
}