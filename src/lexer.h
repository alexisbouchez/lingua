#ifndef LEXER_H
#define LEXER_H

typedef enum {
    TOKEN_IDENT,
    TOKEN_STRING,
    TOKEN_INT,
    TOKEN_FLOAT,
    TOKEN_BOOL,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_SEMICOLON,
    TOKEN_EQUALS,
    TOKEN_COLON,
    TOKEN_EQ,
    TOKEN_NE,
    TOKEN_GT,
    TOKEN_GE,
    TOKEN_LT,
    TOKEN_LE,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_COMMA,
    TOKEN_ARROW,
    TOKEN_AND,
    TOKEN_OR,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_STAR,
    TOKEN_SLASH,
    TOKEN_PERCENT,
    TOKEN_DOT,
    TOKEN_FOR,
    TOKEN_IF,
    TOKEN_ELSE,
    TOKEN_MATCH,
    TOKEN_FAT_ARROW,
    TOKEN_BREAK,
    TOKEN_CONTINUE,
    TOKEN_AMP,
    TOKEN_PIPE,
    TOKEN_CARET,
    TOKEN_TILDE,
    TOKEN_SHL,
    TOKEN_SHR,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_PLUS_EQ,
    TOKEN_MINUS_EQ,
    TOKEN_STAR_EQ,
    TOKEN_SLASH_EQ,
    TOKEN_PERCENT_EQ,
    TOKEN_AMP_EQ,
    TOKEN_PIPE_EQ,
    TOKEN_CARET_EQ,
    TOKEN_SHL_EQ,
    TOKEN_SHR_EQ,
    TOKEN_PLUS_PLUS,
    TOKEN_MINUS_MINUS,
    TOKEN_IMPORT,
    TOKEN_FROM,
    TOKEN_PUB,
    TOKEN_SPAWN,
    TOKEN_EOF,
} TokenType;

typedef struct {
    TokenType type;
    const char *start;
    int length;
    int line;   /* 1-based */
    int col;    /* 1-based */
} Token;

typedef struct {
    const char *source;
    int pos;
    int line;   /* 1-based, current position */
    int col;    /* 1-based, current position */
} Lexer;

void lexer_init(Lexer *l, const char *source);
Token lexer_next(Lexer *l);
Token lexer_peek(Lexer *l);

#endif
