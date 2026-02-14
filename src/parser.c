#include "parser.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static Token expect(Lexer *lexer, TokenType type, const char *what) {
    Token tok = lexer_next(lexer);
    if (tok.type != type) {
        fprintf(stderr, "error: expected %s\n", what);
        exit(1);
    }
    return tok;
}

ASTNode *parse(Lexer *lexer) {
    ASTNode *head = NULL;
    ASTNode *tail = NULL;

    for (;;) {
        Token tok = lexer_next(lexer);
        if (tok.type == TOKEN_EOF)
            break;

        if (tok.type == TOKEN_IDENT && tok.length == 3 &&
            memcmp(tok.start, "let", 3) == 0) {
            /* let <ident> = <string> ; */
            Token name = expect(lexer, TOKEN_IDENT, "variable name");
            expect(lexer, TOKEN_EQUALS, "'='");
            Token str = expect(lexer, TOKEN_STRING, "string literal");
            expect(lexer, TOKEN_SEMICOLON, "';'");

            ASTNode *node = malloc(sizeof(ASTNode));
            node->type = NODE_LET;
            node->var_name = malloc(name.length + 1);
            memcpy(node->var_name, name.start, name.length);
            node->var_name[name.length] = '\0';
            node->string = malloc(str.length + 1);
            memcpy(node->string, str.start, str.length);
            node->string[str.length] = '\0';
            node->string_len = str.length;
            node->is_var_ref = 0;
            node->next = NULL;

            if (tail) {
                tail->next = node;
            } else {
                head = node;
            }
            tail = node;
        } else if (tok.type == TOKEN_IDENT && tok.length == 5 &&
            memcmp(tok.start, "print", 5) == 0) {
            expect(lexer, TOKEN_LPAREN, "'('");

            Token arg = lexer_next(lexer);
            ASTNode *node = malloc(sizeof(ASTNode));
            node->type = NODE_PRINT;
            node->next = NULL;

            if (arg.type == TOKEN_STRING) {
                node->string = malloc(arg.length + 1);
                memcpy(node->string, arg.start, arg.length);
                node->string[arg.length] = '\0';
                node->string_len = arg.length;
                node->var_name = NULL;
                node->is_var_ref = 0;
            } else if (arg.type == TOKEN_IDENT) {
                node->string = NULL;
                node->string_len = 0;
                node->var_name = malloc(arg.length + 1);
                memcpy(node->var_name, arg.start, arg.length);
                node->var_name[arg.length] = '\0';
                node->is_var_ref = 1;
            } else {
                fprintf(stderr, "error: expected string literal or variable name\n");
                exit(1);
            }

            expect(lexer, TOKEN_RPAREN, "')'");
            expect(lexer, TOKEN_SEMICOLON, "';'");

            if (tail) {
                tail->next = node;
            } else {
                head = node;
            }
            tail = node;
        } else {
            fprintf(stderr, "error: unexpected token\n");
            exit(1);
        }
    }

    return head;
}

void ast_free(ASTNode *node) {
    while (node) {
        ASTNode *next = node->next;
        free(node->string);
        free(node->var_name);
        free(node);
        node = next;
    }
}
