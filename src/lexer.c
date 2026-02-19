#include "lexer.h"
#include "diagnostic.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void lexer_init(Lexer *l, const char *source) {
    l->source = source;
    l->pos = 0;
    l->line = 1;
    l->col = 1;
}

static void lexer_advance(Lexer *l) {
    if (l->source[l->pos] == '\n') {
        l->line++;
        l->col = 1;
    } else {
        l->col++;
    }
    l->pos++;
}

static void lexer_advance_n(Lexer *l, int n) {
    for (int i = 0; i < n; i++)
        lexer_advance(l);
}

static void skip_whitespace(Lexer *l) {
    for (;;) {
        while (l->source[l->pos] && isspace(l->source[l->pos]))
            lexer_advance(l);
        if (l->source[l->pos] == '/' && l->source[l->pos + 1] == '/') {
            lexer_advance_n(l, 2);
            while (l->source[l->pos] && l->source[l->pos] != '\n')
                lexer_advance(l);
            continue;
        }
        if (l->source[l->pos] == '/' && l->source[l->pos + 1] == '*') {
            lexer_advance_n(l, 2);
            while (l->source[l->pos]) {
                if (l->source[l->pos] == '*' && l->source[l->pos + 1] == '/') {
                    lexer_advance_n(l, 2);
                    break;
                }
                lexer_advance(l);
            }
            if (!l->source[l->pos] && !(l->source[l->pos - 1] == '/' && l->source[l->pos - 2] == '*')) {
                diag_emit((SourceLoc){l->line, l->col}, DIAG_ERROR, "unterminated block comment");
            }
            continue;
        }
        break;
    }
}

/* Stamp the token's location from the lexer's current position */
#define STAMP_LOC(tok, l) do { (tok).line = (l)->line; (tok).col = (l)->col; } while(0)

Token lexer_next(Lexer *l) {
    skip_whitespace(l);

    Token tok;
    char c = l->source[l->pos];

    if (c == '\0') {
        tok.type = TOKEN_EOF;
        tok.start = &l->source[l->pos];
        tok.length = 0;
        STAMP_LOC(tok, l);
        return tok;
    }

    if (c == '(') {
        tok.type = TOKEN_LPAREN;
        tok.start = &l->source[l->pos];
        tok.length = 1;
        STAMP_LOC(tok, l);
        lexer_advance(l);
        return tok;
    }

    if (c == ')') {
        tok.type = TOKEN_RPAREN;
        tok.start = &l->source[l->pos];
        tok.length = 1;
        STAMP_LOC(tok, l);
        lexer_advance(l);
        return tok;
    }

    if (c == ';') {
        tok.type = TOKEN_SEMICOLON;
        tok.start = &l->source[l->pos];
        tok.length = 1;
        STAMP_LOC(tok, l);
        lexer_advance(l);
        return tok;
    }

    if (c == '=') {
        tok.start = &l->source[l->pos];
        STAMP_LOC(tok, l);
        if (l->source[l->pos + 1] == '>') {
            tok.type = TOKEN_FAT_ARROW;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else if (l->source[l->pos + 1] == '=') {
            tok.type = TOKEN_EQ;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else {
            tok.type = TOKEN_EQUALS;
            tok.length = 1;
            lexer_advance(l);
        }
        return tok;
    }

    if (c == '!') {
        tok.start = &l->source[l->pos];
        STAMP_LOC(tok, l);
        if (l->source[l->pos + 1] == '=') {
            tok.type = TOKEN_NE;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else {
            diag_emit((SourceLoc){l->line, l->col}, DIAG_ERROR, "unexpected character '!'");
        }
        return tok;
    }

    if (c == '>') {
        tok.start = &l->source[l->pos];
        STAMP_LOC(tok, l);
        if (l->source[l->pos + 1] == '>') {
            if (l->source[l->pos + 2] == '=') {
                tok.type = TOKEN_SHR_EQ;
                tok.length = 3;
                lexer_advance_n(l, 3);
            } else {
                tok.type = TOKEN_SHR;
                tok.length = 2;
                lexer_advance_n(l, 2);
            }
        } else if (l->source[l->pos + 1] == '=') {
            tok.type = TOKEN_GE;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else {
            tok.type = TOKEN_GT;
            tok.length = 1;
            lexer_advance(l);
        }
        return tok;
    }

    if (c == '<') {
        tok.start = &l->source[l->pos];
        STAMP_LOC(tok, l);
        if (l->source[l->pos + 1] == '<') {
            if (l->source[l->pos + 2] == '=') {
                tok.type = TOKEN_SHL_EQ;
                tok.length = 3;
                lexer_advance_n(l, 3);
            } else {
                tok.type = TOKEN_SHL;
                tok.length = 2;
                lexer_advance_n(l, 2);
            }
        } else if (l->source[l->pos + 1] == '=') {
            tok.type = TOKEN_LE;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else {
            tok.type = TOKEN_LT;
            tok.length = 1;
            lexer_advance(l);
        }
        return tok;
    }

    if (c == '{') {
        tok.type = TOKEN_LBRACE;
        tok.start = &l->source[l->pos];
        tok.length = 1;
        STAMP_LOC(tok, l);
        lexer_advance(l);
        return tok;
    }

    if (c == '}') {
        tok.type = TOKEN_RBRACE;
        tok.start = &l->source[l->pos];
        tok.length = 1;
        STAMP_LOC(tok, l);
        lexer_advance(l);
        return tok;
    }

    if (c == ',') {
        tok.type = TOKEN_COMMA;
        tok.start = &l->source[l->pos];
        tok.length = 1;
        STAMP_LOC(tok, l);
        lexer_advance(l);
        return tok;
    }

    if (c == '-') {
        tok.start = &l->source[l->pos];
        STAMP_LOC(tok, l);
        if (l->source[l->pos + 1] == '>') {
            tok.type = TOKEN_ARROW;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else if (l->source[l->pos + 1] == '-') {
            tok.type = TOKEN_MINUS_MINUS;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else if (l->source[l->pos + 1] == '=') {
            tok.type = TOKEN_MINUS_EQ;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else {
            tok.type = TOKEN_MINUS;
            tok.length = 1;
            lexer_advance(l);
        }
        return tok;
    }

    if (c == '+') {
        tok.start = &l->source[l->pos];
        STAMP_LOC(tok, l);
        if (l->source[l->pos + 1] == '+') {
            tok.type = TOKEN_PLUS_PLUS;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else if (l->source[l->pos + 1] == '=') {
            tok.type = TOKEN_PLUS_EQ;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else {
            tok.type = TOKEN_PLUS;
            tok.length = 1;
            lexer_advance(l);
        }
        return tok;
    }

    if (c == '*') {
        tok.start = &l->source[l->pos];
        STAMP_LOC(tok, l);
        if (l->source[l->pos + 1] == '=') {
            tok.type = TOKEN_STAR_EQ;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else {
            tok.type = TOKEN_STAR;
            tok.length = 1;
            lexer_advance(l);
        }
        return tok;
    }

    if (c == '/') {
        tok.start = &l->source[l->pos];
        STAMP_LOC(tok, l);
        if (l->source[l->pos + 1] == '=') {
            tok.type = TOKEN_SLASH_EQ;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else {
            tok.type = TOKEN_SLASH;
            tok.length = 1;
            lexer_advance(l);
        }
        return tok;
    }

    if (c == '%') {
        tok.start = &l->source[l->pos];
        STAMP_LOC(tok, l);
        if (l->source[l->pos + 1] == '=') {
            tok.type = TOKEN_PERCENT_EQ;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else {
            tok.type = TOKEN_PERCENT;
            tok.length = 1;
            lexer_advance(l);
        }
        return tok;
    }

    if (c == '.') {
        tok.type = TOKEN_DOT;
        tok.start = &l->source[l->pos];
        tok.length = 1;
        STAMP_LOC(tok, l);
        lexer_advance(l);
        return tok;
    }

    if (c == ':') {
        tok.type = TOKEN_COLON;
        tok.start = &l->source[l->pos];
        tok.length = 1;
        STAMP_LOC(tok, l);
        lexer_advance(l);
        return tok;
    }

    if (c == '&') {
        tok.start = &l->source[l->pos];
        STAMP_LOC(tok, l);
        if (l->source[l->pos + 1] == '=') {
            tok.type = TOKEN_AMP_EQ;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else {
            tok.type = TOKEN_AMP;
            tok.length = 1;
            lexer_advance(l);
        }
        return tok;
    }

    if (c == '|') {
        tok.start = &l->source[l->pos];
        STAMP_LOC(tok, l);
        if (l->source[l->pos + 1] == '=') {
            tok.type = TOKEN_PIPE_EQ;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else {
            tok.type = TOKEN_PIPE;
            tok.length = 1;
            lexer_advance(l);
        }
        return tok;
    }

    if (c == '^') {
        tok.start = &l->source[l->pos];
        STAMP_LOC(tok, l);
        if (l->source[l->pos + 1] == '=') {
            tok.type = TOKEN_CARET_EQ;
            tok.length = 2;
            lexer_advance_n(l, 2);
        } else {
            tok.type = TOKEN_CARET;
            tok.length = 1;
            lexer_advance(l);
        }
        return tok;
    }

    if (c == '~') {
        tok.type = TOKEN_TILDE;
        tok.start = &l->source[l->pos];
        tok.length = 1;
        STAMP_LOC(tok, l);
        lexer_advance(l);
        return tok;
    }

    if (c == '[') {
        tok.type = TOKEN_LBRACKET;
        tok.start = &l->source[l->pos];
        tok.length = 1;
        STAMP_LOC(tok, l);
        lexer_advance(l);
        return tok;
    }

    if (c == ']') {
        tok.type = TOKEN_RBRACKET;
        tok.start = &l->source[l->pos];
        tok.length = 1;
        STAMP_LOC(tok, l);
        lexer_advance(l);
        return tok;
    }

    if (c == '"') {
        STAMP_LOC(tok, l);
        lexer_advance(l); /* skip opening quote */
        int start = l->pos;
        while (l->source[l->pos] && l->source[l->pos] != '"') {
            if (l->source[l->pos] == '\\' && l->source[l->pos + 1])
                lexer_advance(l); /* skip backslash */
            lexer_advance(l);
        }
        tok.type = TOKEN_STRING;
        tok.start = &l->source[start];
        tok.length = l->pos - start;
        if (l->source[l->pos] == '"')
            lexer_advance(l); /* skip closing quote */
        return tok;
    }

    if (isdigit(c)) {
        int start = l->pos;
        STAMP_LOC(tok, l);
        if (c == '0' && (l->source[l->pos + 1] == 'x' || l->source[l->pos + 1] == 'X')) {
            lexer_advance_n(l, 2); /* skip 0x */
            while (isxdigit(l->source[l->pos]))
                lexer_advance(l);
            tok.type = TOKEN_INT;
        } else {
            while (isdigit(l->source[l->pos]))
                lexer_advance(l);
            if (l->source[l->pos] == '.' && isdigit(l->source[l->pos + 1])) {
                lexer_advance(l); /* skip '.' */
                while (isdigit(l->source[l->pos]))
                    lexer_advance(l);
                tok.type = TOKEN_FLOAT;
            } else {
                tok.type = TOKEN_INT;
            }
        }
        tok.start = &l->source[start];
        tok.length = l->pos - start;
        return tok;
    }

    if (isalpha(c) || c == '_') {
        int start = l->pos;
        STAMP_LOC(tok, l);
        while (isalnum(l->source[l->pos]) || l->source[l->pos] == '_')
            lexer_advance(l);
        tok.start = &l->source[start];
        tok.length = l->pos - start;
        if (tok.length == 3 && memcmp(tok.start, "and", 3) == 0) {
            tok.type = TOKEN_AND;
        } else if (tok.length == 2 && memcmp(tok.start, "or", 2) == 0) {
            tok.type = TOKEN_OR;
        } else if ((tok.length == 4 && memcmp(tok.start, "true", 4) == 0) ||
            (tok.length == 5 && memcmp(tok.start, "false", 5) == 0)) {
            tok.type = TOKEN_BOOL;
        } else if (tok.length == 3 && memcmp(tok.start, "for", 3) == 0) {
            tok.type = TOKEN_FOR;
        } else if (tok.length == 2 && memcmp(tok.start, "if", 2) == 0) {
            tok.type = TOKEN_IF;
        } else if (tok.length == 4 && memcmp(tok.start, "else", 4) == 0) {
            tok.type = TOKEN_ELSE;
        } else if (tok.length == 5 && memcmp(tok.start, "match", 5) == 0) {
            tok.type = TOKEN_MATCH;
        } else if (tok.length == 5 && memcmp(tok.start, "break", 5) == 0) {
            tok.type = TOKEN_BREAK;
        } else if (tok.length == 8 && memcmp(tok.start, "continue", 8) == 0) {
            tok.type = TOKEN_CONTINUE;
        } else if (tok.length == 6 && memcmp(tok.start, "import", 6) == 0) {
            tok.type = TOKEN_IMPORT;
        } else if (tok.length == 4 && memcmp(tok.start, "from", 4) == 0) {
            tok.type = TOKEN_FROM;
        } else if (tok.length == 3 && memcmp(tok.start, "pub", 3) == 0) {
            tok.type = TOKEN_PUB;
        } else if (tok.length == 5 && memcmp(tok.start, "spawn", 5) == 0) {
            tok.type = TOKEN_SPAWN;
        } else {
            tok.type = TOKEN_IDENT;
        }
        return tok;
    }

    /* Unknown character — skip and return EOF */
    tok.type = TOKEN_EOF;
    tok.start = &l->source[l->pos];
    tok.length = 0;
    STAMP_LOC(tok, l);
    lexer_advance(l);
    return tok;
}

Token lexer_peek(Lexer *l) {
    int saved_pos = l->pos;
    int saved_line = l->line;
    int saved_col = l->col;
    Token tok = lexer_next(l);
    l->pos = saved_pos;
    l->line = saved_line;
    l->col = saved_col;
    return tok;
}
