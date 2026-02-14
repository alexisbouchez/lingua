#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"

typedef enum {
    NODE_PRINT,
    NODE_LET,
} NodeType;

typedef struct ASTNode {
    NodeType type;
    char *string;
    int string_len;
    char *var_name;
    int is_var_ref;
    struct ASTNode *next;
} ASTNode;

ASTNode *parse(Lexer *lexer);
void ast_free(ASTNode *node);

#endif
