#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "diagnostic.h"

typedef enum {
    NODE_PRINT,
    NODE_VAR_DECL,
    NODE_ASSIGN,
    NODE_FN_DECL,
    NODE_FN_CALL,
    NODE_RETURN,
    NODE_FOR_LOOP,
    NODE_IF_STMT,
    NODE_MATCH_STMT,
    NODE_BLOCK,
    NODE_CLASS_DECL,
    NODE_BREAK,
    NODE_CONTINUE,
    NODE_IMPORT,
} NodeType;

typedef enum {
    VAL_STRING,
    VAL_INT,
    VAL_FLOAT,
    VAL_BOOL,
    VAL_VOID,
    VAL_OBJECT,
} ValueType;

typedef enum {
    EXPR_INT_LIT,
    EXPR_FLOAT_LIT,
    EXPR_STRING_LIT,
    EXPR_BOOL_LIT,
    EXPR_VAR_REF,
    EXPR_BINARY,
    EXPR_MEMBER_ACCESS,
    EXPR_UNARY,
    EXPR_INDEX,
    EXPR_SLICE,
    EXPR_FN_CALL,
} ExprKind;

typedef enum {
    BINOP_ADD,
    BINOP_SUB,
    BINOP_MUL,
    BINOP_DIV,
    BINOP_MOD,
    BINOP_EQ,
    BINOP_NE,
    BINOP_GT,
    BINOP_GE,
    BINOP_LT,
    BINOP_LE,
    BINOP_AND,
    BINOP_OR,
    BINOP_BIT_AND,
    BINOP_BIT_OR,
    BINOP_BIT_XOR,
    BINOP_SHL,
    BINOP_SHR,
} BinOpKind;

typedef enum { UNOP_NEG, UNOP_BIT_NOT } UnaryOpKind;

typedef struct Expr {
    ExprKind kind;
    ValueType value_type;
    SourceLoc loc;
    union {
        struct { long value; } int_lit;
        struct { double value; } float_lit;
        struct { char *value; int len; } string_lit;
        struct { int value; } bool_lit;
        struct { char *name; } var_ref;
        struct { BinOpKind op; struct Expr *left; struct Expr *right; } binary;
        struct { struct Expr *object; char *field_name; } member_access;
        struct { UnaryOpKind op; struct Expr *operand; } unary;
        struct { struct Expr *object; struct Expr *index; } index_access;
        struct { struct Expr *object; struct Expr *start; struct Expr *end; } slice;
        struct {
            char *fn_name;
            char *obj_name;     /* NULL for free functions, set for obj.method() */
            struct Expr **args;
            char **arg_names;   /* NULL entry = positional */
            int arg_count;
        } fn_call;
    } as;
} Expr;

typedef struct {
    char *name;
    ValueType type;
    char *class_type_name;
    int has_default;
    char *default_value;
    int default_value_len;
} FnParam;

typedef struct {
    char *name;
    ValueType type;
    char *class_type_name;
} ClassField;

typedef struct {
    Expr *pattern;    /* NULL for wildcard _ */
    int is_wildcard;
    struct ASTNode *body;
} MatchArm;

typedef struct ASTNode {
    NodeType type;
    SourceLoc loc;
    char *var_name;
    int is_const;
    Expr *expr;

    /* Function declaration fields */
    char *fn_name;
    FnParam *params;
    int param_count;
    int has_return_type;
    ValueType return_type;
    struct ASTNode *body;

    /* For loop fields */
    struct ASTNode *for_init;
    Expr *for_cond;
    struct ASTNode *for_update;

    /* If statement fields */
    Expr *if_cond;
    struct ASTNode *if_body;
    struct ASTNode *else_body;

    /* Match statement fields */
    Expr *match_expr;
    MatchArm *match_arms;
    int match_arm_count;

    /* Print fields */
    int print_newline; /* 1 = append \n (default), 0 = no trailing newline */

    /* Function call fields */
    int is_fn_call;
    char **call_args;
    ValueType *call_arg_types;
    int *call_arg_is_var_ref;
    char **call_arg_names;
    int call_arg_count;
    struct Expr **call_arg_exprs; /* expression-based arguments (NULL entries = use call_args) */

    /* Class declaration fields */
    char *class_name;
    char *parent_class_name;
    ClassField *class_fields;
    int class_field_count;
    struct ASTNode *class_methods;

    /* Object/method/field support */
    char *obj_name;      /* for method calls: obj.method(...) */
    char *field_name;    /* for dotted assignment: obj.field = value */
    int is_new_expr;     /* for new ClassName(...) */

    /* Import fields */
    char *import_path;       /* module path string */
    char **import_names;     /* array of imported symbol names */
    int import_name_count;

    /* Pub visibility */
    int is_pub;

    struct ASTNode *next;
} ASTNode;

ASTNode *parse(Lexer *lexer);
void ast_free(ASTNode *node);
void expr_free(Expr *expr);
const char *value_type_name(ValueType vt);

#endif
