// Placeholder for lexer tests

#[cfg(test)]
mod tests {
use lingua::lexer::{Lexer, token::Token};
use lingua::lexer::token::Token::*;

#[test]
fn test_tokenize_minimal_function() {
    let input = "fn main() -> i32 { 42 }";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    // Expect: [Fn, Ident("main"), LParen, RParen, Arrow, I32, LBrace, Int(42), RBrace, Eof]
    assert_eq!(
        tokens,
        vec![
            Token::Fn,
            Token::Ident("main".to_string()),
            Token::LParen,
            Token::RParen,
            Token::Arrow,
            Token::I32,
            Token::LBrace,
            Token::Int(42),
            Token::RBrace,
            Token::Eof
        ]
    );
}

#[test]
fn test_tokenize_empty() {
    let lexer = Lexer::new("");
    let tokens = lexer.tokenize();
    assert_eq!(tokens, vec![]);
}

#[test]
fn test_tokenize_keywords() {
    let input = "fn let mut if else loop while return use pub struct enum impl trait type const static match";
    let lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_eq!(
        tokens,
        vec![
            Token::Fn,
            Token::Let,
            Token::Mut,
            Token::If,
            Token::Else,
            Token::Loop,
            Token::While,
            Token::Return,
            Token::Use,
            Token::Pub,
            Token::Struct,
            Token::Enum,
            Token::Impl,
            Token::Trait,
            Token::Type,
            Token::Const,
            Token::Static,
            Token::Match,
            Token::Eof
        ]
    );
}

#[test]
fn test_tokenize_types() {
    let input = "i32 i64 f32 f64 bool";
    let lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_eq!(
        tokens,
        vec![
            Token::I32,
            Token::I64,
            Token::F32,
            Token::F64,
            Token::Bool,
            Token::Eof
        ]
    );
}

#[test]
fn test_tokenize_literals() {
    let input = "42 3.14 true false";
    let lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_eq!(
        tokens,
        vec![
            Token::Int(42),
            Token::Float(3.14),
            Token::True,
            Token::False,
            Token::Eof
        ]
    );
}

#[test]
fn test_tokenize_operators() {
    let input = "+ - * / % = == != < <= > >= && || ! & | ^ << >>";
    let lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_eq!(
        tokens,
        vec![
            Token::Plus,
            Token::Minus,
            Token::Asterisk,
            Token::Slash,
            Token::Percent,
            Token::Eq,
            Token::EqEq,
            Token::NotEq,
            Token::Lt,
            Token::LtEq,
            Token::Gt,
            Token::GtEq,
            Token::AmpersandAmpersand,
            Token::PipePipe,
            Token::Bang,
            Token::Ampersand,
            Token::Pipe,
            Token::Caret,
            Token::LtLt,
            Token::GtGt,
            Token::Eof
        ]
    );
}

#[test]
fn test_tokenize_punctuation() {
    let input = "-> => : ; , . .. :: ( ) { } [ ]";
    let lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_eq!(
        tokens,
        vec![
            Token::Arrow,
            Token::FatArrow,
            Token::Colon,
            Token::Semicolon,
            Token::Comma,
            Token::Period,
            Token::Range,
            Token::ColonColon,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::LBracket,
            Token::RBracket,
            Token::Eof
        ]
    );
}

#[test]
fn test_tokenize_identifiers() {
    let input = "foo bar baz _qux123";
    let lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_eq!(
        tokens,
        vec![
            Token::Ident("foo".to_string()),
            Token::Ident("bar".to_string()),
            Token::Ident("baz".to_string()),
            Token::Ident("_qux123".to_string()),
            Token::Eof
        ]
    );
}

#[test]
fn test_tokenize_strings() {
    let input = "\"hello\" \"world\"";
    let lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_eq!(
        tokens,
        vec![
            Token::String("hello".to_string()),
            Token::String("world".to_string()),
            Token::Eof
        ]
    );
}

#[test]
fn test_token_spans() {
    let input = "fn main() -> i32 { 42 }";
    let lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_eq!(
        tokens,
        vec![
            Token::Fn,
            Token::Ident("main".to_string()),
            Token::LParen,
            Token::RParen,
            Token::Arrow,
            Token::I32,
            Token::LBrace,
            Token::Int(42),
            Token::RBrace,
            Token::Eof
        ]
    );
}

}