#include "parser.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Helper to save/restore lexer state for lookahead */
typedef struct {
    int pos;
    int line;
    int col;
} LexerState;

static LexerState lexer_save(Lexer *l) {
    return (LexerState){l->pos, l->line, l->col};
}

static void lexer_restore(Lexer *l, LexerState s) {
    l->pos = s.pos;
    l->line = s.line;
    l->col = s.col;
}

const char *value_type_name(ValueType vt) {
    switch (vt) {
        case VAL_STRING: return "string";
        case VAL_INT:    return "int";
        case VAL_FLOAT:  return "float";
        case VAL_BOOL:   return "bool";
        case VAL_VOID:   return "void";
        case VAL_OBJECT: return "object";
        case VAL_ARRAY:   return "Array";
        case VAL_CHANNEL: return "Channel";
        default:          return "unknown";
    }
}

/* Process escape sequences in a string token, returning a new malloc'd buffer.
   Sets *out_len to the length of the processed string. */
static char *process_escapes(const char *raw, int raw_len, int *out_len, SourceLoc loc) {
    char *buf = malloc(raw_len + 1);
    int j = 0;
    for (int i = 0; i < raw_len; i++) {
        if (raw[i] == '\\' && i + 1 < raw_len) {
            i++;
            switch (raw[i]) {
                case 'n':  buf[j++] = '\n'; break;
                case 't':  buf[j++] = '\t'; break;
                case 'r':  buf[j++] = '\r'; break;
                case '\\': buf[j++] = '\\'; break;
                case '"':  buf[j++] = '"';  break;
                case '0':  buf[j++] = '\0'; break;
                case '{':  buf[j++] = '{';  break;
                case '}':  buf[j++] = '}';  break;
                default:
                    free(buf);
                    diag_emit(loc, DIAG_ERROR, "unknown escape sequence '\\%c'", raw[i]);
            }
        } else {
            buf[j++] = raw[i];
        }
    }
    buf[j] = '\0';
    *out_len = j;
    return buf;
}

/* Forward declaration */
static Token expect(Lexer *lexer, TokenType type, const char *what);

/* Parse a type name token and return its ValueType.
   If out_class_name is non-NULL and the type is a class name, *out_class_name
   is set to a malloc'd copy of the class name. */
static ValueType parse_type_name(Token *tok, char **out_class_name) {
    if (out_class_name) *out_class_name = NULL;
    if (tok->length == 3 && memcmp(tok->start, "int", 3) == 0)
        return VAL_INT;
    if (tok->length == 5 && memcmp(tok->start, "float", 5) == 0)
        return VAL_FLOAT;
    if (tok->length == 6 && memcmp(tok->start, "string", 6) == 0)
        return VAL_STRING;
    if (tok->length == 4 && memcmp(tok->start, "bool", 4) == 0)
        return VAL_BOOL;
    /* Treat unrecognized type names as class types (codegen verifies) */
    if (out_class_name) {
        *out_class_name = malloc(tok->length + 1);
        memcpy(*out_class_name, tok->start, tok->length);
        (*out_class_name)[tok->length] = '\0';
    }
    return VAL_OBJECT;
}

static Token expect(Lexer *lexer, TokenType type, const char *what) {
    Token tok = lexer_next(lexer);
    if (tok.type != type) {
        diag_emit((SourceLoc){tok.line, tok.col}, DIAG_ERROR, "expected %s", what);
    }
    return tok;
}

/* Parse a type that may be Array<T>. Reads additional tokens from lexer if needed.
   type_tok is the already-consumed identifier token. */
static ValueType parse_full_type(Lexer *lexer, Token *type_tok, char **out_class_name,
                                 ValueType *out_array_elem_type) {
    if (out_array_elem_type) *out_array_elem_type = VAL_VOID;
    if (type_tok->length == 7 && memcmp(type_tok->start, "Channel", 7) == 0) {
        Token peek = lexer_peek(lexer);
        if (peek.type == TOKEN_LT) {
            lexer_next(lexer); /* consume '<' */
            Token elem_tok = expect(lexer, TOKEN_IDENT, "element type");
            expect(lexer, TOKEN_GT, "'>'");
            ValueType elem_type = parse_type_name(&elem_tok, NULL);
            if (elem_type == VAL_OBJECT)
                diag_emit((SourceLoc){elem_tok.line, elem_tok.col}, DIAG_ERROR,
                          "Channel element type must be int, float, string, or bool");
            if (out_array_elem_type) *out_array_elem_type = elem_type;
            if (out_class_name) *out_class_name = NULL;
            return VAL_CHANNEL;
        }
    }
    if (type_tok->length == 5 && memcmp(type_tok->start, "Array", 5) == 0) {
        Token peek = lexer_peek(lexer);
        if (peek.type == TOKEN_LT) {
            lexer_next(lexer); /* consume '<' */
            Token elem_tok = expect(lexer, TOKEN_IDENT, "element type");
            expect(lexer, TOKEN_GT, "'>'");
            ValueType elem_type = parse_type_name(&elem_tok, NULL);
            if (elem_type == VAL_OBJECT)
                diag_emit((SourceLoc){elem_tok.line, elem_tok.col}, DIAG_ERROR,
                          "Array element type must be int, float, string, or bool");
            if (out_array_elem_type) *out_array_elem_type = elem_type;
            if (out_class_name) *out_class_name = NULL;
            return VAL_ARRAY;
        }
    }
    return parse_type_name(type_tok, out_class_name);
}

/* ================================================================
 * Expr allocation helpers
 * ================================================================ */

static Expr *expr_alloc(ExprKind kind) {
    Expr *e = malloc(sizeof(Expr));
    memset(e, 0, sizeof(Expr));
    e->kind = kind;
    return e;
}

void expr_free(Expr *expr) {
    if (!expr) return;
    switch (expr->kind) {
        case EXPR_STRING_LIT:
            free(expr->as.string_lit.value);
            break;
        case EXPR_VAR_REF:
            free(expr->as.var_ref.name);
            break;
        case EXPR_BINARY:
            expr_free(expr->as.binary.left);
            expr_free(expr->as.binary.right);
            break;
        case EXPR_MEMBER_ACCESS:
            expr_free(expr->as.member_access.object);
            free(expr->as.member_access.field_name);
            break;
        case EXPR_UNARY:
            expr_free(expr->as.unary.operand);
            break;
        case EXPR_INDEX:
            expr_free(expr->as.index_access.object);
            expr_free(expr->as.index_access.index);
            break;
        case EXPR_SLICE:
            expr_free(expr->as.slice.object);
            expr_free(expr->as.slice.start);
            expr_free(expr->as.slice.end);
            break;
        case EXPR_FN_CALL:
            free(expr->as.fn_call.fn_name);
            free(expr->as.fn_call.obj_name);
            if (expr->as.fn_call.args) {
                for (int i = 0; i < expr->as.fn_call.arg_count; i++)
                    expr_free(expr->as.fn_call.args[i]);
                free(expr->as.fn_call.args);
            }
            if (expr->as.fn_call.arg_names) {
                for (int i = 0; i < expr->as.fn_call.arg_count; i++)
                    free(expr->as.fn_call.arg_names[i]);
                free(expr->as.fn_call.arg_names);
            }
            break;
        case EXPR_ARRAY_LIT:
            if (expr->as.array_lit.elements) {
                for (int i = 0; i < expr->as.array_lit.count; i++)
                    expr_free(expr->as.array_lit.elements[i]);
                free(expr->as.array_lit.elements);
            }
            break;
        case EXPR_CHANNEL_LIT:
            break;
        default:
            break;
    }
    free(expr);
}

/* ================================================================
 * Recursive descent expression parser
 *
 * parse_expr()           → parse_or()
 * parse_or()             → parse_and() ['or' parse_and()]
 * parse_and()            → parse_comparison() ['and' parse_comparison()]
 * parse_comparison()     → parse_bitor() [('=='|'!='|...) parse_bitor()]
 * parse_bitor()          → parse_bitxor() ('|' parse_bitxor())*
 * parse_bitxor()         → parse_bitand() ('^' parse_bitand())*
 * parse_bitand()         → parse_shift() ('&' parse_shift())*
 * parse_shift()          → parse_additive() (('<<'|'>>') parse_additive())*
 * parse_additive()       → parse_multiplicative() (('+'|'-') parse_multiplicative())*
 * parse_multiplicative() → parse_unary() (('*'|'/'|'%') parse_unary())*
 * parse_unary()          → ['-'|'~'] parse_postfix()
 * parse_postfix()        → parse_primary() (('.' IDENT) | ('[' expr ']'))*
 * parse_primary()        → INT | FLOAT | STRING | BOOL | IDENT | '(' parse_expr() ')'
 * ================================================================ */

static Expr *parse_expr(Lexer *lexer);

/* Build a BINOP_ADD chain from string segments + interpolated expressions.
   The raw token text (before escape processing) is scanned for unescaped '{'.
   Segments between interpolations are string literals; {expr} parts are parsed
   by creating a sub-lexer. */
static Expr *parse_interpolated_string(const char *raw, int raw_len, SourceLoc loc) {
    Expr *result = NULL;
    int i = 0;

    while (i < raw_len) {
        /* Find next unescaped '{' */
        int seg_start = i;
        while (i < raw_len) {
            if (raw[i] == '\\' && i + 1 < raw_len) {
                i += 2; /* skip escaped char */
                continue;
            }
            if (raw[i] == '{') break;
            i++;
        }

        /* Emit string segment before '{' (may be empty) */
        if (i > seg_start) {
            int slen;
            char *processed = process_escapes(raw + seg_start, i - seg_start, &slen, loc);
            Expr *seg = expr_alloc(EXPR_STRING_LIT);
            seg->loc = loc;
            seg->value_type = VAL_STRING;
            seg->as.string_lit.value = processed;
            seg->as.string_lit.len = slen;
            if (result) {
                Expr *add = expr_alloc(EXPR_BINARY);
                add->loc = loc;
                add->as.binary.op = BINOP_ADD;
                add->as.binary.left = result;
                add->as.binary.right = seg;
                result = add;
            } else {
                result = seg;
            }
        }

        if (i >= raw_len) break;

        /* We're at '{' — find matching '}' */
        i++; /* skip '{' */
        int expr_start = i;
        int depth = 1;
        while (i < raw_len && depth > 0) {
            if (raw[i] == '{') depth++;
            else if (raw[i] == '}') depth--;
            if (depth > 0) i++;
        }
        if (depth != 0)
            diag_emit(loc, DIAG_ERROR, "unterminated interpolation in string");

        int expr_len = i - expr_start;
        i++; /* skip '}' */

        /* Parse the expression text */
        char *expr_text = malloc(expr_len + 1);
        memcpy(expr_text, raw + expr_start, expr_len);
        expr_text[expr_len] = '\0';

        Lexer sub;
        lexer_init(&sub, expr_text);
        Expr *inner = parse_expr(&sub);
        free(expr_text);

        if (result) {
            Expr *add = expr_alloc(EXPR_BINARY);
            add->loc = loc;
            add->as.binary.op = BINOP_ADD;
            add->as.binary.left = result;
            add->as.binary.right = inner;
            result = add;
        } else {
            result = inner;
        }
    }

    if (!result) {
        /* Empty string */
        result = expr_alloc(EXPR_STRING_LIT);
        result->loc = loc;
        result->value_type = VAL_STRING;
        result->as.string_lit.value = malloc(1);
        result->as.string_lit.value[0] = '\0';
        result->as.string_lit.len = 0;
    }

    return result;
}

static Expr *parse_primary(Lexer *lexer) {
    Token tok = lexer_next(lexer);
    SourceLoc loc = {tok.line, tok.col};

    if (tok.type == TOKEN_INT) {
        Expr *e = expr_alloc(EXPR_INT_LIT);
        e->loc = loc;
        e->kind = EXPR_INT_LIT;
        e->value_type = VAL_INT;
        char buf[32];
        int len = tok.length < 31 ? tok.length : 31;
        memcpy(buf, tok.start, len);
        buf[len] = '\0';
        e->as.int_lit.value = strtol(buf, NULL, 0);
        return e;
    }

    if (tok.type == TOKEN_FLOAT) {
        Expr *e = expr_alloc(EXPR_FLOAT_LIT);
        e->loc = loc;
        e->value_type = VAL_FLOAT;
        char buf[64];
        int len = tok.length < 63 ? tok.length : 63;
        memcpy(buf, tok.start, len);
        buf[len] = '\0';
        e->as.float_lit.value = atof(buf);
        return e;
    }

    if (tok.type == TOKEN_STRING) {
        /* Check for string interpolation: unescaped '{' in raw token text */
        int has_interp = 0;
        for (int j = 0; j < tok.length; j++) {
            if (tok.start[j] == '\\' && j + 1 < tok.length) { j++; continue; }
            if (tok.start[j] == '{') { has_interp = 1; break; }
        }
        if (has_interp) {
            return parse_interpolated_string(tok.start, tok.length, loc);
        }
        Expr *e = expr_alloc(EXPR_STRING_LIT);
        e->loc = loc;
        e->value_type = VAL_STRING;
        e->as.string_lit.value = process_escapes(tok.start, tok.length, &e->as.string_lit.len, loc);
        return e;
    }

    if (tok.type == TOKEN_BOOL) {
        Expr *e = expr_alloc(EXPR_BOOL_LIT);
        e->loc = loc;
        e->value_type = VAL_BOOL;
        e->as.bool_lit.value = (tok.length == 4 && memcmp(tok.start, "true", 4) == 0) ? 1 : 0;
        return e;
    }

    if (tok.type == TOKEN_IDENT) {
        /* channel<T>() expression */
        if (tok.length == 7 && memcmp(tok.start, "channel", 7) == 0) {
            Token peek = lexer_peek(lexer);
            if (peek.type == TOKEN_LT) {
                lexer_next(lexer); /* consume '<' */
                Token elem_tok = expect(lexer, TOKEN_IDENT, "element type");
                expect(lexer, TOKEN_GT, "'>'");
                expect(lexer, TOKEN_LPAREN, "'('");
                expect(lexer, TOKEN_RPAREN, "')'");
                ValueType elem_type = parse_type_name(&elem_tok, NULL);
                if (elem_type == VAL_OBJECT)
                    diag_emit((SourceLoc){elem_tok.line, elem_tok.col}, DIAG_ERROR,
                              "channel element type must be int, float, string, or bool");
                Expr *e = expr_alloc(EXPR_CHANNEL_LIT);
                e->loc = loc;
                e->value_type = VAL_CHANNEL;
                e->as.channel_lit.elem_type = elem_type;
                return e;
            }
        }
        Expr *e = expr_alloc(EXPR_VAR_REF);
        e->loc = loc;
        e->as.var_ref.name = malloc(tok.length + 1);
        memcpy(e->as.var_ref.name, tok.start, tok.length);
        e->as.var_ref.name[tok.length] = '\0';
        return e;
    }

    if (tok.type == TOKEN_LPAREN) {
        Expr *e = parse_expr(lexer);
        expect(lexer, TOKEN_RPAREN, "')'");
        return e;
    }

    if (tok.type == TOKEN_LBRACKET) {
        Expr *e = expr_alloc(EXPR_ARRAY_LIT);
        e->loc = loc;
        e->value_type = VAL_ARRAY;
        int cap = 4;
        e->as.array_lit.elements = malloc(cap * sizeof(Expr *));
        e->as.array_lit.count = 0;
        Token peek = lexer_peek(lexer);
        if (peek.type != TOKEN_RBRACKET) {
            for (;;) {
                if (e->as.array_lit.count == cap) {
                    cap *= 2;
                    e->as.array_lit.elements = realloc(e->as.array_lit.elements, cap * sizeof(Expr *));
                }
                e->as.array_lit.elements[e->as.array_lit.count++] = parse_expr(lexer);
                peek = lexer_peek(lexer);
                if (peek.type == TOKEN_COMMA) {
                    lexer_next(lexer);
                } else {
                    break;
                }
            }
        }
        expect(lexer, TOKEN_RBRACKET, "']'");
        return e;
    }

    diag_emit(loc, DIAG_ERROR, "expected expression");
    return NULL; /* unreachable */
}

/* Parse function call argument list as expressions (for EXPR_FN_CALL).
   Assumes opening '(' is already consumed. Consumes closing ')'. */
static void parse_expr_call_args(Lexer *lexer, Expr ***out_args, char ***out_names, int *out_count) {
    int cap = 4;
    *out_args = malloc(cap * sizeof(Expr *));
    *out_names = malloc(cap * sizeof(char *));
    *out_count = 0;
    int seen_named = 0;

    Token peek = lexer_peek(lexer);
    if (peek.type != TOKEN_RPAREN) {
        for (;;) {
            if (*out_count == cap) {
                cap *= 2;
                *out_args = realloc(*out_args, cap * sizeof(Expr *));
                *out_names = realloc(*out_names, cap * sizeof(char *));
            }

            /* Check for named argument: IDENT COLON */
            LexerState saved = lexer_save(lexer);
            Token first = lexer_next(lexer);
            if (first.type == TOKEN_IDENT) {
                Token after = lexer_peek(lexer);
                if (after.type == TOKEN_COLON) {
                    lexer_next(lexer); /* consume ':' */
                    seen_named = 1;
                    (*out_names)[*out_count] = malloc(first.length + 1);
                    memcpy((*out_names)[*out_count], first.start, first.length);
                    (*out_names)[*out_count][first.length] = '\0';
                    (*out_args)[*out_count] = parse_expr(lexer);
                    (*out_count)++;
                    goto next_expr_arg;
                }
            }

            /* Positional argument */
            lexer_restore(lexer, saved);
            if (seen_named)
                diag_emit((SourceLoc){first.line, first.col}, DIAG_ERROR, "positional argument after named argument");
            (*out_names)[*out_count] = NULL;
            (*out_args)[*out_count] = parse_expr(lexer);
            (*out_count)++;

next_expr_arg:
            peek = lexer_peek(lexer);
            if (peek.type == TOKEN_COMMA) {
                lexer_next(lexer);
            } else {
                break;
            }
        }
    }
    expect(lexer, TOKEN_RPAREN, "')'");
}

static Expr *parse_postfix(Lexer *lexer) {
    Expr *left = parse_primary(lexer);
    for (;;) {
        Token peek = lexer_peek(lexer);
        if (peek.type == TOKEN_DOT) {
            lexer_next(lexer); /* consume '.' */
            Token field = expect(lexer, TOKEN_IDENT, "field name");
            Token after_field = lexer_peek(lexer);
            if (after_field.type == TOKEN_LPAREN) {
                /* Method call: left.method(args) */
                lexer_next(lexer); /* consume '(' */
                /* left must be a VAR_REF for method calls */
                if (left->kind != EXPR_VAR_REF)
                    diag_emit(left->loc, DIAG_ERROR, "method calls only supported on variables");
                Expr *call = expr_alloc(EXPR_FN_CALL);
                call->loc = (SourceLoc){field.line, field.col};
                call->as.fn_call.fn_name = malloc(field.length + 1);
                memcpy(call->as.fn_call.fn_name, field.start, field.length);
                call->as.fn_call.fn_name[field.length] = '\0';
                call->as.fn_call.obj_name = left->as.var_ref.name;
                left->as.var_ref.name = NULL; /* transfer ownership */
                expr_free(left);
                parse_expr_call_args(lexer, &call->as.fn_call.args,
                                     &call->as.fn_call.arg_names, &call->as.fn_call.arg_count);
                left = call;
                continue;
            }
            Expr *ma = expr_alloc(EXPR_MEMBER_ACCESS);
            ma->loc = (SourceLoc){field.line, field.col};
            ma->as.member_access.object = left;
            ma->as.member_access.field_name = malloc(field.length + 1);
            memcpy(ma->as.member_access.field_name, field.start, field.length);
            ma->as.member_access.field_name[field.length] = '\0';
            left = ma;
        } else if (peek.type == TOKEN_LPAREN && left->kind == EXPR_VAR_REF) {
            /* Function call: name(args) */
            lexer_next(lexer); /* consume '(' */
            Expr *call = expr_alloc(EXPR_FN_CALL);
            call->loc = left->loc;
            call->as.fn_call.fn_name = left->as.var_ref.name;
            call->as.fn_call.obj_name = NULL;
            left->as.var_ref.name = NULL; /* transfer ownership */
            expr_free(left);
            parse_expr_call_args(lexer, &call->as.fn_call.args,
                                 &call->as.fn_call.arg_names, &call->as.fn_call.arg_count);
            left = call;
        } else if (peek.type == TOKEN_LBRACKET) {
            Token bracket_tok = lexer_next(lexer); /* consume '[' */
            SourceLoc bracket_loc = {bracket_tok.line, bracket_tok.col};
            Expr *index = parse_expr(lexer);
            Token after = lexer_peek(lexer);
            if (after.type == TOKEN_COLON) {
                /* Slice: obj[start:end] */
                lexer_next(lexer); /* consume ':' */
                Expr *end = parse_expr(lexer);
                expect(lexer, TOKEN_RBRACKET, "']'");
                Expr *sl = expr_alloc(EXPR_SLICE);
                sl->loc = bracket_loc;
                sl->as.slice.object = left;
                sl->as.slice.start = index;
                sl->as.slice.end = end;
                left = sl;
            } else {
                /* Index: obj[index] */
                expect(lexer, TOKEN_RBRACKET, "']'");
                Expr *idx = expr_alloc(EXPR_INDEX);
                idx->loc = bracket_loc;
                idx->as.index_access.object = left;
                idx->as.index_access.index = index;
                left = idx;
            }
        } else {
            break;
        }
    }
    return left;
}

static Expr *parse_unary(Lexer *lexer) {
    Token peek = lexer_peek(lexer);
    if (peek.type == TOKEN_MINUS) {
        Token minus_tok = lexer_next(lexer); /* consume '-' */
        SourceLoc loc = {minus_tok.line, minus_tok.col};
        Expr *operand = parse_unary(lexer);
        /* Optimize: negate literal directly */
        if (operand->kind == EXPR_INT_LIT) {
            operand->as.int_lit.value = -operand->as.int_lit.value;
            operand->loc = loc;
            return operand;
        }
        if (operand->kind == EXPR_FLOAT_LIT) {
            operand->as.float_lit.value = -operand->as.float_lit.value;
            operand->loc = loc;
            return operand;
        }
        /* General case: UNOP_NEG */
        Expr *u = expr_alloc(EXPR_UNARY);
        u->loc = loc;
        u->as.unary.op = UNOP_NEG;
        u->as.unary.operand = operand;
        return u;
    }
    if (peek.type == TOKEN_TILDE) {
        Token tilde_tok = lexer_next(lexer); /* consume '~' */
        SourceLoc loc = {tilde_tok.line, tilde_tok.col};
        Expr *operand = parse_unary(lexer);
        Expr *u = expr_alloc(EXPR_UNARY);
        u->loc = loc;
        u->as.unary.op = UNOP_BIT_NOT;
        u->as.unary.operand = operand;
        return u;
    }
    return parse_postfix(lexer);
}

static Expr *parse_multiplicative(Lexer *lexer) {
    Expr *left = parse_unary(lexer);
    for (;;) {
        Token peek = lexer_peek(lexer);
        BinOpKind op;
        if (peek.type == TOKEN_STAR) op = BINOP_MUL;
        else if (peek.type == TOKEN_SLASH) op = BINOP_DIV;
        else if (peek.type == TOKEN_PERCENT) op = BINOP_MOD;
        else break;
        Token op_tok = lexer_next(lexer); /* consume operator */
        Expr *right = parse_unary(lexer);
        Expr *bin = expr_alloc(EXPR_BINARY);
        bin->loc = (SourceLoc){op_tok.line, op_tok.col};
        bin->as.binary.op = op;
        bin->as.binary.left = left;
        bin->as.binary.right = right;
        left = bin;
    }
    return left;
}

static Expr *parse_additive(Lexer *lexer) {
    Expr *left = parse_multiplicative(lexer);
    for (;;) {
        Token peek = lexer_peek(lexer);
        BinOpKind op;
        if (peek.type == TOKEN_PLUS) op = BINOP_ADD;
        else if (peek.type == TOKEN_MINUS) op = BINOP_SUB;
        else break;
        Token op_tok = lexer_next(lexer); /* consume operator */
        Expr *right = parse_multiplicative(lexer);
        Expr *bin = expr_alloc(EXPR_BINARY);
        bin->loc = (SourceLoc){op_tok.line, op_tok.col};
        bin->as.binary.op = op;
        bin->as.binary.left = left;
        bin->as.binary.right = right;
        left = bin;
    }
    return left;
}

static Expr *parse_shift(Lexer *lexer) {
    Expr *left = parse_additive(lexer);
    for (;;) {
        Token peek = lexer_peek(lexer);
        BinOpKind op;
        if (peek.type == TOKEN_SHL) op = BINOP_SHL;
        else if (peek.type == TOKEN_SHR) op = BINOP_SHR;
        else break;
        Token op_tok = lexer_next(lexer);
        Expr *right = parse_additive(lexer);
        Expr *bin = expr_alloc(EXPR_BINARY);
        bin->loc = (SourceLoc){op_tok.line, op_tok.col};
        bin->as.binary.op = op;
        bin->as.binary.left = left;
        bin->as.binary.right = right;
        left = bin;
    }
    return left;
}

static Expr *parse_bitand(Lexer *lexer) {
    Expr *left = parse_shift(lexer);
    for (;;) {
        Token peek = lexer_peek(lexer);
        if (peek.type != TOKEN_AMP) break;
        Token op_tok = lexer_next(lexer);
        Expr *right = parse_shift(lexer);
        Expr *bin = expr_alloc(EXPR_BINARY);
        bin->loc = (SourceLoc){op_tok.line, op_tok.col};
        bin->as.binary.op = BINOP_BIT_AND;
        bin->as.binary.left = left;
        bin->as.binary.right = right;
        left = bin;
    }
    return left;
}

static Expr *parse_bitxor(Lexer *lexer) {
    Expr *left = parse_bitand(lexer);
    for (;;) {
        Token peek = lexer_peek(lexer);
        if (peek.type != TOKEN_CARET) break;
        Token op_tok = lexer_next(lexer);
        Expr *right = parse_bitand(lexer);
        Expr *bin = expr_alloc(EXPR_BINARY);
        bin->loc = (SourceLoc){op_tok.line, op_tok.col};
        bin->as.binary.op = BINOP_BIT_XOR;
        bin->as.binary.left = left;
        bin->as.binary.right = right;
        left = bin;
    }
    return left;
}

static Expr *parse_bitor(Lexer *lexer) {
    Expr *left = parse_bitxor(lexer);
    for (;;) {
        Token peek = lexer_peek(lexer);
        if (peek.type != TOKEN_PIPE) break;
        Token op_tok = lexer_next(lexer);
        Expr *right = parse_bitxor(lexer);
        Expr *bin = expr_alloc(EXPR_BINARY);
        bin->loc = (SourceLoc){op_tok.line, op_tok.col};
        bin->as.binary.op = BINOP_BIT_OR;
        bin->as.binary.left = left;
        bin->as.binary.right = right;
        left = bin;
    }
    return left;
}

static Expr *parse_comparison(Lexer *lexer) {
    Expr *left = parse_bitor(lexer);
    Token peek = lexer_peek(lexer);
    BinOpKind op;
    int is_cmp = 1;
    switch (peek.type) {
        case TOKEN_EQ: op = BINOP_EQ; break;
        case TOKEN_NE: op = BINOP_NE; break;
        case TOKEN_GT: op = BINOP_GT; break;
        case TOKEN_GE: op = BINOP_GE; break;
        case TOKEN_LT: op = BINOP_LT; break;
        case TOKEN_LE: op = BINOP_LE; break;
        default: is_cmp = 0; break;
    }
    if (is_cmp) {
        Token op_tok = lexer_next(lexer); /* consume operator */
        Expr *right = parse_bitor(lexer);
        Expr *bin = expr_alloc(EXPR_BINARY);
        bin->loc = (SourceLoc){op_tok.line, op_tok.col};
        bin->as.binary.op = op;
        bin->as.binary.left = left;
        bin->as.binary.right = right;
        left = bin;
    }
    return left;
}

static Expr *parse_and(Lexer *lexer) {
    Expr *left = parse_comparison(lexer);
    Token peek = lexer_peek(lexer);
    if (peek.type == TOKEN_AND) {
        Token op_tok = lexer_next(lexer);
        Expr *right = parse_comparison(lexer);
        Expr *bin = expr_alloc(EXPR_BINARY);
        bin->loc = (SourceLoc){op_tok.line, op_tok.col};
        bin->as.binary.op = BINOP_AND;
        bin->as.binary.left = left;
        bin->as.binary.right = right;
        left = bin;
    }
    return left;
}

static Expr *parse_or(Lexer *lexer) {
    Expr *left = parse_and(lexer);
    Token peek = lexer_peek(lexer);
    if (peek.type == TOKEN_OR) {
        Token op_tok = lexer_next(lexer);
        Expr *right = parse_and(lexer);
        Expr *bin = expr_alloc(EXPR_BINARY);
        bin->loc = (SourceLoc){op_tok.line, op_tok.col};
        bin->as.binary.op = BINOP_OR;
        bin->as.binary.left = left;
        bin->as.binary.right = right;
        left = bin;
    }
    return left;
}

static Expr *parse_expr(Lexer *lexer) {
    return parse_or(lexer);
}

/* ================================================================
 * Function call argument parsing (unchanged, still flat for now)
 * ================================================================ */

/* Helper: copy token text into a malloc'd string */
static char *token_strdup(Token *tok, int *out_len) {
    char *s = malloc(tok->length + 1);
    memcpy(s, tok->start, tok->length);
    s[tok->length] = '\0';
    *out_len = tok->length;
    return s;
}


static ValueType token_to_value_type(TokenType t) {
    switch (t) {
        case TOKEN_INT:    return VAL_INT;
        case TOKEN_FLOAT:  return VAL_FLOAT;
        case TOKEN_BOOL:   return VAL_BOOL;
        case TOKEN_STRING: return VAL_STRING;
        default:           return VAL_STRING;
    }
}


static void parse_call_args(Lexer *lexer, ASTNode *node) {
    int arg_cap = 4;
    node->call_args = malloc(arg_cap * sizeof(char *));
    node->call_arg_types = malloc(arg_cap * sizeof(ValueType));
    node->call_arg_is_var_ref = malloc(arg_cap * sizeof(int));
    node->call_arg_names = malloc(arg_cap * sizeof(char *));
    node->call_arg_exprs = malloc(arg_cap * sizeof(Expr *));
    node->call_arg_count = 0;

    int seen_named = 0;

    Token peek = lexer_peek(lexer);
    if (peek.type != TOKEN_RPAREN) {
        for (;;) {
            if (node->call_arg_count == arg_cap) {
                arg_cap *= 2;
                node->call_args = realloc(node->call_args, arg_cap * sizeof(char *));
                node->call_arg_types = realloc(node->call_arg_types, arg_cap * sizeof(ValueType));
                node->call_arg_is_var_ref = realloc(node->call_arg_is_var_ref, arg_cap * sizeof(int));
                node->call_arg_names = realloc(node->call_arg_names, arg_cap * sizeof(char *));
                node->call_arg_exprs = realloc(node->call_arg_exprs, arg_cap * sizeof(Expr *));
            }

            /* Check for named argument: IDENT COLON (not ==, !=, etc.) */
            LexerState saved = lexer_save(lexer);
            Token first = lexer_next(lexer);
            SourceLoc first_loc = {first.line, first.col};
            if (first.type == TOKEN_IDENT) {
                Token after = lexer_peek(lexer);
                if (after.type == TOKEN_COLON) {
                    /* Named argument: name: expr */
                    lexer_next(lexer); /* consume ':' */
                    seen_named = 1;
                    node->call_arg_names[node->call_arg_count] = malloc(first.length + 1);
                    memcpy(node->call_arg_names[node->call_arg_count], first.start, first.length);
                    node->call_arg_names[node->call_arg_count][first.length] = '\0';
                    node->call_args[node->call_arg_count] = NULL;
                    node->call_arg_types[node->call_arg_count] = VAL_VOID;
                    node->call_arg_is_var_ref[node->call_arg_count] = 0;
                    node->call_arg_exprs[node->call_arg_count] = parse_expr(lexer);
                    node->call_arg_count++;
                    goto next_arg;
                }
            }

            /* Positional argument: parse as expression */
            lexer_restore(lexer, saved);
            if (seen_named) {
                diag_emit(first_loc, DIAG_ERROR, "positional argument after named argument");
            }
            node->call_arg_names[node->call_arg_count] = NULL;
            node->call_args[node->call_arg_count] = NULL;
            node->call_arg_types[node->call_arg_count] = VAL_VOID;
            node->call_arg_is_var_ref[node->call_arg_count] = 0;
            node->call_arg_exprs[node->call_arg_count] = parse_expr(lexer);
            node->call_arg_count++;

next_arg:
            peek = lexer_peek(lexer);
            if (peek.type == TOKEN_COMMA) {
                lexer_next(lexer);
            } else {
                break;
            }
        }
    }
    expect(lexer, TOKEN_RPAREN, "')'");
}

/* ================================================================
 * Statement parsing
 * ================================================================ */

/* Forward declarations */
static ASTNode *parse_statement(Lexer *lexer, Token tok);
static int try_parse_rhs_call(Lexer *lexer, ASTNode *node);
static int try_parse_new_only(Lexer *lexer, ASTNode *node);

/* Loop depth counter for break/continue validation */
static int parse_loop_depth = 0;

/* Scope depth counter for pub/import top-level enforcement */
static int parse_scope_depth = 0;

/* Parse an import statement: import { name1, name2 } from "path"; */
static ASTNode *parse_import_stmt(Lexer *lexer, SourceLoc import_loc) {
    expect(lexer, TOKEN_LBRACE, "'{'");

    int cap = 4;
    char **names = malloc(cap * sizeof(char *));
    int count = 0;

    Token peek = lexer_peek(lexer);
    if (peek.type != TOKEN_RBRACE) {
        for (;;) {
            if (count == cap) {
                cap *= 2;
                names = realloc(names, cap * sizeof(char *));
            }
            Token name = expect(lexer, TOKEN_IDENT, "import name");
            names[count] = malloc(name.length + 1);
            memcpy(names[count], name.start, name.length);
            names[count][name.length] = '\0';
            count++;

            peek = lexer_peek(lexer);
            if (peek.type == TOKEN_COMMA) {
                lexer_next(lexer);
            } else {
                break;
            }
        }
    }
    expect(lexer, TOKEN_RBRACE, "'}'");
    expect(lexer, TOKEN_FROM, "'from'");
    Token path = expect(lexer, TOKEN_STRING, "module path string");
    expect(lexer, TOKEN_SEMICOLON, "';'");

    ASTNode *node = malloc(sizeof(ASTNode));
    memset(node, 0, sizeof(ASTNode));
    node->type = NODE_IMPORT;
    node->loc = import_loc;
    node->import_names = names;
    node->import_name_count = count;

    /* Process escape sequences in path (though paths shouldn't need them) */
    int path_len;
    node->import_path = malloc(path.length + 1);
    memcpy(node->import_path, path.start, path.length);
    node->import_path[path.length] = '\0';
    (void)path_len;

    return node;
}

/* Parse a function declaration: fn NAME(params) [-> TYPE] { body } */
static ASTNode *parse_fn_decl(Lexer *lexer, SourceLoc fn_loc) {
    Token name = expect(lexer, TOKEN_IDENT, "function name");

    ASTNode *node = malloc(sizeof(ASTNode));
    memset(node, 0, sizeof(ASTNode));
    node->type = NODE_FN_DECL;
    node->loc = fn_loc;
    node->fn_name = malloc(name.length + 1);
    memcpy(node->fn_name, name.start, name.length);
    node->fn_name[name.length] = '\0';

    /* Parse parameter list */
    expect(lexer, TOKEN_LPAREN, "'('");

    int param_cap = 4;
    node->params = malloc(param_cap * sizeof(FnParam));
    node->param_count = 0;

    int seen_default = 0;

    Token peek = lexer_peek(lexer);
    if (peek.type != TOKEN_RPAREN) {
        for (;;) {
            if (node->param_count == param_cap) {
                param_cap *= 2;
                node->params = realloc(node->params, param_cap * sizeof(FnParam));
            }
            Token pname = expect(lexer, TOKEN_IDENT, "parameter name");
            expect(lexer, TOKEN_COLON, "':'");
            Token ptype = expect(lexer, TOKEN_IDENT, "parameter type");

            FnParam *p = &node->params[node->param_count];
            p->name = malloc(pname.length + 1);
            memcpy(p->name, pname.start, pname.length);
            p->name[pname.length] = '\0';
            p->type = parse_full_type(lexer, &ptype, &p->class_type_name, &p->array_elem_type);
            p->has_default = 0;
            p->default_value = NULL;
            p->default_value_len = 0;

            /* Check for default value: = <literal> */
            peek = lexer_peek(lexer);
            if (peek.type == TOKEN_EQUALS) {
                lexer_next(lexer); /* consume '=' */
                seen_default = 1;
                p->has_default = 1;

                Token def = lexer_next(lexer);
                SourceLoc def_loc = {def.line, def.col};
                if (def.type == TOKEN_STRING) {
                    p->default_value = process_escapes(def.start, def.length, &p->default_value_len, def_loc);
                } else if (def.type == TOKEN_INT || def.type == TOKEN_FLOAT || def.type == TOKEN_BOOL) {
                    p->default_value = token_strdup(&def, &p->default_value_len);
                } else {
                    diag_emit(def_loc, DIAG_ERROR, "expected literal for default parameter value");
                }

                /* Validate default type matches param type */
                ValueType def_type = token_to_value_type(def.type);
                if (def.type == TOKEN_STRING) def_type = VAL_STRING;
                if (def_type != p->type) {
                    diag_emit(def_loc, DIAG_ERROR, "default value type '%s' does not match parameter '%s' type '%s'",
                            value_type_name(def_type), p->name, value_type_name(p->type));
                }
            } else if (seen_default) {
                diag_emit((SourceLoc){pname.line, pname.col}, DIAG_ERROR,
                          "required parameter '%s' after parameter with default value", p->name);
            }

            node->param_count++;

            peek = lexer_peek(lexer);
            if (peek.type == TOKEN_COMMA) {
                lexer_next(lexer);
            } else {
                break;
            }
        }
    }
    expect(lexer, TOKEN_RPAREN, "')'");

    /* Parse optional return type: -> TYPE or -> Array<T> */
    peek = lexer_peek(lexer);
    if (peek.type == TOKEN_ARROW) {
        lexer_next(lexer); /* consume '->' */
        Token ret_type = expect(lexer, TOKEN_IDENT, "return type");
        node->has_return_type = 1;
        node->return_type = parse_full_type(lexer, &ret_type, NULL, &node->return_array_elem_type);
    } else {
        node->has_return_type = 0;
        node->return_type = VAL_VOID;
    }

    /* Parse body: { statements } or shorthand: return <expr>; */
    peek = lexer_peek(lexer);
    if (peek.type == TOKEN_IDENT && peek.length == 6 && memcmp(peek.start, "return", 6) == 0) {
        /* Shorthand: fn name(params) [-> type] return <expr>; */
        Token ret_tok = lexer_next(lexer); /* consume 'return' */
        ASTNode *ret_node = malloc(sizeof(ASTNode));
        memset(ret_node, 0, sizeof(ASTNode));
        ret_node->type = NODE_RETURN;
        ret_node->loc = (SourceLoc){ret_tok.line, ret_tok.col};

        if (!try_parse_new_only(lexer, ret_node)) {
            ret_node->expr = parse_expr(lexer);
        }

        expect(lexer, TOKEN_SEMICOLON, "';'");
        node->body = ret_node;
        return node;
    }

    expect(lexer, TOKEN_LBRACE, "'{'");

    parse_scope_depth++;
    ASTNode *body_head = NULL;
    ASTNode *body_tail = NULL;

    for (;;) {
        peek = lexer_peek(lexer);
        if (peek.type == TOKEN_RBRACE) {
            lexer_next(lexer); /* consume '}' */
            break;
        }
        if (peek.type == TOKEN_EOF) {
            diag_emit((SourceLoc){peek.line, peek.col}, DIAG_ERROR, "unexpected end of file in function body");
        }

        Token stmt_tok = lexer_next(lexer);
        ASTNode *stmt = parse_statement(lexer, stmt_tok);
        if (stmt) {
            if (body_tail) {
                body_tail->next = stmt;
            } else {
                body_head = stmt;
            }
            body_tail = stmt;
        }
    }
    parse_scope_depth--;

    node->body = body_head;
    return node;
}

/* Parse a class declaration: class Name [extends Parent] { fields; methods } */
static ASTNode *parse_class_decl(Lexer *lexer, SourceLoc class_loc) {
    Token name = expect(lexer, TOKEN_IDENT, "class name");

    ASTNode *node = malloc(sizeof(ASTNode));
    memset(node, 0, sizeof(ASTNode));
    node->type = NODE_CLASS_DECL;
    node->loc = class_loc;
    node->class_name = malloc(name.length + 1);
    memcpy(node->class_name, name.start, name.length);
    node->class_name[name.length] = '\0';

    /* Optional: extends ParentClass */
    Token peek = lexer_peek(lexer);
    if (peek.type == TOKEN_IDENT && peek.length == 7 && memcmp(peek.start, "extends", 7) == 0) {
        lexer_next(lexer); /* consume 'extends' */
        Token parent = expect(lexer, TOKEN_IDENT, "parent class name");
        node->parent_class_name = malloc(parent.length + 1);
        memcpy(node->parent_class_name, parent.start, parent.length);
        node->parent_class_name[parent.length] = '\0';
    }

    expect(lexer, TOKEN_LBRACE, "'{'");

    int field_cap = 4;
    node->class_fields = malloc(field_cap * sizeof(ClassField));
    node->class_field_count = 0;
    ASTNode *method_head = NULL, *method_tail = NULL;

    for (;;) {
        peek = lexer_peek(lexer);
        if (peek.type == TOKEN_RBRACE) { lexer_next(lexer); break; }
        if (peek.type == TOKEN_EOF)
            diag_emit((SourceLoc){peek.line, peek.col}, DIAG_ERROR, "unexpected end of file in class body");

        /* Check for method: fn ... */
        if (peek.type == TOKEN_IDENT && peek.length == 2 && memcmp(peek.start, "fn", 2) == 0) {
            Token fn_tok = lexer_next(lexer);
            ASTNode *method = parse_fn_decl(lexer, (SourceLoc){fn_tok.line, fn_tok.col});
            if (method_tail) { method_tail->next = method; } else { method_head = method; }
            method_tail = method;
            continue;
        }

        /* Field: name: type; */
        Token fname = expect(lexer, TOKEN_IDENT, "field name or 'fn'");
        expect(lexer, TOKEN_COLON, "':'");
        Token ftype = expect(lexer, TOKEN_IDENT, "field type");
        expect(lexer, TOKEN_SEMICOLON, "';'");

        if (node->class_field_count == field_cap) {
            field_cap *= 2;
            node->class_fields = realloc(node->class_fields, field_cap * sizeof(ClassField));
        }
        ClassField *f = &node->class_fields[node->class_field_count++];
        f->name = malloc(fname.length + 1);
        memcpy(f->name, fname.start, fname.length);
        f->name[fname.length] = '\0';
        f->type = parse_type_name(&ftype, &f->class_type_name);
    }

    node->class_methods = method_head;
    return node;
}

/* Parse an enum declaration: enum Name { Variant [= int], ... } */
static ASTNode *parse_enum_decl(Lexer *lexer, SourceLoc enum_loc) {
    Token name = expect(lexer, TOKEN_IDENT, "enum name");

    ASTNode *node = malloc(sizeof(ASTNode));
    memset(node, 0, sizeof(ASTNode));
    node->type = NODE_ENUM_DECL;
    node->loc = enum_loc;
    node->enum_name = malloc(name.length + 1);
    memcpy(node->enum_name, name.start, name.length);
    node->enum_name[name.length] = '\0';

    expect(lexer, TOKEN_LBRACE, "'{'");

    int variant_cap = 4;
    node->enum_variants = malloc(variant_cap * sizeof(EnumVariant));
    node->enum_variant_count = 0;
    long next_value = 0;

    for (;;) {
        Token peek = lexer_peek(lexer);
        if (peek.type == TOKEN_RBRACE) { lexer_next(lexer); break; }
        if (peek.type == TOKEN_EOF)
            diag_emit((SourceLoc){peek.line, peek.col}, DIAG_ERROR, "unexpected end of file in enum body");

        Token vname = expect(lexer, TOKEN_IDENT, "variant name");

        if (node->enum_variant_count == variant_cap) {
            variant_cap *= 2;
            node->enum_variants = realloc(node->enum_variants, variant_cap * sizeof(EnumVariant));
        }

        EnumVariant *v = &node->enum_variants[node->enum_variant_count];
        v->name = malloc(vname.length + 1);
        memcpy(v->name, vname.start, vname.length);
        v->name[vname.length] = '\0';

        /* Check for explicit value: = <int> */
        peek = lexer_peek(lexer);
        if (peek.type == TOKEN_EQUALS) {
            lexer_next(lexer); /* consume '=' */
            Token val_tok = lexer_next(lexer);
            SourceLoc val_loc = {val_tok.line, val_tok.col};
            int negate = 0;
            if (val_tok.type == TOKEN_MINUS) {
                negate = 1;
                val_tok = lexer_next(lexer);
                val_loc = (SourceLoc){val_tok.line, val_tok.col};
            }
            if (val_tok.type != TOKEN_INT)
                diag_emit(val_loc, DIAG_ERROR, "enum variant value must be an integer");
            char buf[32];
            int len = val_tok.length < 31 ? val_tok.length : 31;
            memcpy(buf, val_tok.start, len);
            buf[len] = '\0';
            long val = strtol(buf, NULL, 0);
            v->value = negate ? -val : val;
            next_value = v->value + 1;
        } else {
            v->value = next_value;
            next_value++;
        }

        node->enum_variant_count++;

        /* Expect comma or closing brace */
        peek = lexer_peek(lexer);
        if (peek.type == TOKEN_COMMA) {
            lexer_next(lexer);
        }
    }

    return node;
}

/* Try to parse an RHS that is a function call, method call, or new expression.
   Returns 1 if matched (populates node fields), 0 to fall through to parse_expr.
   Lexer state is restored if no match. */
static int try_parse_rhs_call(Lexer *lexer, ASTNode *node) {
    Token peek = lexer_peek(lexer);
    if (peek.type != TOKEN_IDENT) return 0;

    LexerState saved = lexer_save(lexer);
    Token first = lexer_next(lexer);

    /* Check for 'new ClassName(...)' */
    if (first.length == 3 && memcmp(first.start, "new", 3) == 0) {
        Token cls = lexer_peek(lexer);
        if (cls.type == TOKEN_IDENT) {
            Token cls_tok = lexer_next(lexer);
            Token lp = lexer_peek(lexer);
            if (lp.type == TOKEN_LPAREN) {
                lexer_next(lexer); /* consume '(' */
                node->is_new_expr = 1;
                node->is_fn_call = 1;
                node->fn_name = malloc(cls_tok.length + 1);
                memcpy(node->fn_name, cls_tok.start, cls_tok.length);
                node->fn_name[cls_tok.length] = '\0';
                parse_call_args(lexer, node);
                return 1;
            }
        }
        /* 'new' not followed by Class( — restore */
        lexer_restore(lexer, saved);
        return 0;
    }

    /* Check for IDENT.method(args) or IDENT(args) */
    Token after = lexer_peek(lexer);

    if (after.type == TOKEN_DOT) {
        /* obj.method(args) */
        lexer_next(lexer); /* consume '.' */
        Token method = lexer_peek(lexer);
        if (method.type == TOKEN_IDENT) {
            Token method_tok = lexer_next(lexer);
            Token lp = lexer_peek(lexer);
            if (lp.type == TOKEN_LPAREN) {
                lexer_next(lexer); /* consume '(' */
                node->is_fn_call = 1;
                node->obj_name = malloc(first.length + 1);
                memcpy(node->obj_name, first.start, first.length);
                node->obj_name[first.length] = '\0';
                node->fn_name = malloc(method_tok.length + 1);
                memcpy(node->fn_name, method_tok.start, method_tok.length);
                node->fn_name[method_tok.length] = '\0';
                parse_call_args(lexer, node);
                return 1;
            }
        }
        /* Not a method call, restore */
        lexer_restore(lexer, saved);
        return 0;
    }

    if (after.type == TOKEN_LPAREN) {
        /* Regular function call */
        lexer_next(lexer); /* consume '(' */
        node->is_fn_call = 1;
        node->fn_name = malloc(first.length + 1);
        memcpy(node->fn_name, first.start, first.length);
        node->fn_name[first.length] = '\0';
        parse_call_args(lexer, node);
        return 1;
    }

    /* Not a call, restore */
    lexer_restore(lexer, saved);
    return 0;
}

/* Only try to parse 'new ClassName(...)' — everything else goes through parse_expr */
static int try_parse_new_only(Lexer *lexer, ASTNode *node) {
    Token peek = lexer_peek(lexer);
    if (peek.type == TOKEN_IDENT && peek.length == 3 && memcmp(peek.start, "new", 3) == 0)
        return try_parse_rhs_call(lexer, node);
    return 0;
}

/* Parse a body: either { stmts } or a single statement */
static ASTNode *parse_body(Lexer *lexer, const char *context) {
    Token peek = lexer_peek(lexer);
    if (peek.type == TOKEN_LBRACE) {
        lexer_next(lexer); /* consume '{' */
        parse_scope_depth++;
        ASTNode *head = NULL;
        ASTNode *tail = NULL;
        for (;;) {
            peek = lexer_peek(lexer);
            if (peek.type == TOKEN_RBRACE) {
                lexer_next(lexer);
                break;
            }
            if (peek.type == TOKEN_EOF) {
                diag_emit((SourceLoc){peek.line, peek.col}, DIAG_ERROR,
                          "unexpected end of file in %s body", context);
            }
            Token stmt_tok = lexer_next(lexer);
            ASTNode *stmt = parse_statement(lexer, stmt_tok);
            if (stmt) {
                if (tail) { tail->next = stmt; } else { head = stmt; }
                tail = stmt;
            }
        }
        parse_scope_depth--;
        return head;
    }
    /* Single statement (no braces) */
    parse_scope_depth++;
    Token stmt_tok = lexer_next(lexer);
    ASTNode *result = parse_statement(lexer, stmt_tok);
    parse_scope_depth--;
    return result;
}

/* Helper: build an assign node for compound update parsing (used in for-loop update) */
static ASTNode *parse_update_clause(Lexer *lexer) {
    Token update_ident = expect(lexer, TOKEN_IDENT, "variable name");
    SourceLoc uloc = {update_ident.line, update_ident.col};

    ASTNode *update = malloc(sizeof(ASTNode));
    memset(update, 0, sizeof(ASTNode));
    update->type = NODE_ASSIGN;
    update->loc = uloc;
    update->var_name = malloc(update_ident.length + 1);
    memcpy(update->var_name, update_ident.start, update_ident.length);
    update->var_name[update_ident.length] = '\0';

    Token op = lexer_peek(lexer);

    /* i++ or i-- */
    if (op.type == TOKEN_PLUS_PLUS || op.type == TOKEN_MINUS_MINUS) {
        lexer_next(lexer);
        Expr *var = expr_alloc(EXPR_VAR_REF);
        var->loc = uloc;
        var->as.var_ref.name = malloc(update_ident.length + 1);
        memcpy(var->as.var_ref.name, update_ident.start, update_ident.length);
        var->as.var_ref.name[update_ident.length] = '\0';
        Expr *one = expr_alloc(EXPR_INT_LIT);
        one->loc = uloc;
        one->value_type = VAL_INT;
        one->as.int_lit.value = 1;
        Expr *bin = expr_alloc(EXPR_BINARY);
        bin->loc = uloc;
        bin->as.binary.op = (op.type == TOKEN_PLUS_PLUS) ? BINOP_ADD : BINOP_SUB;
        bin->as.binary.left = var;
        bin->as.binary.right = one;
        update->expr = bin;
        return update;
    }

    /* Compound assignment: +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>= */
    BinOpKind compound_op;
    int is_compound = 1;
    switch (op.type) {
        case TOKEN_PLUS_EQ:    compound_op = BINOP_ADD; break;
        case TOKEN_MINUS_EQ:   compound_op = BINOP_SUB; break;
        case TOKEN_STAR_EQ:    compound_op = BINOP_MUL; break;
        case TOKEN_SLASH_EQ:   compound_op = BINOP_DIV; break;
        case TOKEN_PERCENT_EQ: compound_op = BINOP_MOD; break;
        case TOKEN_AMP_EQ:     compound_op = BINOP_BIT_AND; break;
        case TOKEN_PIPE_EQ:    compound_op = BINOP_BIT_OR; break;
        case TOKEN_CARET_EQ:   compound_op = BINOP_BIT_XOR; break;
        case TOKEN_SHL_EQ:     compound_op = BINOP_SHL; break;
        case TOKEN_SHR_EQ:     compound_op = BINOP_SHR; break;
        default: is_compound = 0; break;
    }

    if (is_compound) {
        lexer_next(lexer); /* consume compound operator */
        Expr *var = expr_alloc(EXPR_VAR_REF);
        var->loc = uloc;
        var->as.var_ref.name = malloc(update_ident.length + 1);
        memcpy(var->as.var_ref.name, update_ident.start, update_ident.length);
        var->as.var_ref.name[update_ident.length] = '\0';
        Expr *rhs = parse_expr(lexer);
        Expr *bin = expr_alloc(EXPR_BINARY);
        bin->loc = uloc;
        bin->as.binary.op = compound_op;
        bin->as.binary.left = var;
        bin->as.binary.right = rhs;
        update->expr = bin;
        return update;
    }

    /* Plain assignment: i = expr */
    expect(lexer, TOKEN_EQUALS, "'=' or '+=' or '++' etc.");
    update->expr = parse_expr(lexer);
    return update;
}

/* Parse a for loop: for (init; cond; update) { body } */
static ASTNode *parse_for_loop(Lexer *lexer, SourceLoc for_loc) {
    expect(lexer, TOKEN_LPAREN, "'('");

    /* Parse init statement: var <name> = <expr>; */
    Token init_tok = lexer_next(lexer);
    ASTNode *init = parse_statement(lexer, init_tok);

    /* Parse condition expression */
    Expr *cond = parse_expr(lexer);
    expect(lexer, TOKEN_SEMICOLON, "';'");

    /* Parse update clause */
    ASTNode *update = parse_update_clause(lexer);

    expect(lexer, TOKEN_RPAREN, "')'");

    parse_loop_depth++;
    ASTNode *body_head = parse_body(lexer, "for loop");
    parse_loop_depth--;

    ASTNode *node = malloc(sizeof(ASTNode));
    memset(node, 0, sizeof(ASTNode));
    node->type = NODE_FOR_LOOP;
    node->loc = for_loc;
    node->for_init = init;
    node->for_cond = cond;
    node->for_update = update;
    node->body = body_head;
    return node;
}

/* Parse an if statement: if <cond> { <body> } [else [if ...] { <body> }] */
static ASTNode *parse_if_stmt(Lexer *lexer, SourceLoc if_loc) {
    expect(lexer, TOKEN_LPAREN, "'('");
    Expr *cond = parse_expr(lexer);
    expect(lexer, TOKEN_RPAREN, "')'");

    ASTNode *body_head = parse_body(lexer, "if");

    ASTNode *else_body = NULL;
    Token peek = lexer_peek(lexer);
    if (peek.type == TOKEN_ELSE) {
        lexer_next(lexer); /* consume 'else' */
        Token after_else = lexer_peek(lexer);
        if (after_else.type == TOKEN_IF) {
            /* else if: recurse */
            Token if_tok = lexer_next(lexer); /* consume 'if' */
            else_body = parse_if_stmt(lexer, (SourceLoc){if_tok.line, if_tok.col});
        } else {
            else_body = parse_body(lexer, "else");
        }
    }

    ASTNode *node = malloc(sizeof(ASTNode));
    memset(node, 0, sizeof(ASTNode));
    node->type = NODE_IF_STMT;
    node->loc = if_loc;
    node->if_cond = cond;
    node->if_body = body_head;
    node->else_body = else_body;
    return node;
}

/* Parse a match statement: match (<expr>) { <arms> } */
static ASTNode *parse_match_stmt(Lexer *lexer, SourceLoc match_loc) {
    expect(lexer, TOKEN_LPAREN, "'('");
    Expr *scrutinee = parse_expr(lexer);
    expect(lexer, TOKEN_RPAREN, "')'");
    expect(lexer, TOKEN_LBRACE, "'{'");

    int arm_cap = 4;
    int arm_count = 0;
    MatchArm *arms = malloc(arm_cap * sizeof(MatchArm));

    for (;;) {
        Token peek = lexer_peek(lexer);
        if (peek.type == TOKEN_RBRACE) {
            lexer_next(lexer);
            break;
        }
        if (peek.type == TOKEN_EOF) {
            diag_emit((SourceLoc){peek.line, peek.col}, DIAG_ERROR, "unexpected end of file in match body");
        }

        if (arm_count == arm_cap) {
            arm_cap *= 2;
            arms = realloc(arms, arm_cap * sizeof(MatchArm));
        }

        MatchArm *arm = &arms[arm_count];
        memset(arm, 0, sizeof(MatchArm));

        /* Check for wildcard _ */
        peek = lexer_peek(lexer);
        if (peek.type == TOKEN_IDENT && peek.length == 1 && peek.start[0] == '_') {
            lexer_next(lexer); /* consume _ */
            arm->is_wildcard = 1;
            arm->pattern = NULL;
        } else {
            arm->is_wildcard = 0;
            arm->pattern = parse_expr(lexer);
        }

        expect(lexer, TOKEN_FAT_ARROW, "'=>'");

        arm->body = parse_body(lexer, "match arm");
        arm_count++;
    }

    ASTNode *node = malloc(sizeof(ASTNode));
    memset(node, 0, sizeof(ASTNode));
    node->type = NODE_MATCH_STMT;
    node->loc = match_loc;
    node->match_expr = scrutinee;
    node->match_arms = arms;
    node->match_arm_count = arm_count;
    return node;
}

/* Parse a single statement given the first token already consumed. */
static ASTNode *parse_statement(Lexer *lexer, Token tok) {
    SourceLoc stmt_loc = {tok.line, tok.col};

    if (tok.type == TOKEN_PUB) {
        if (parse_scope_depth > 0)
            diag_emit(stmt_loc, DIAG_ERROR, "'pub' can only be used at the top level");
        Token next = lexer_next(lexer);
        ASTNode *inner = parse_statement(lexer, next);
        if (inner) inner->is_pub = 1;
        return inner;
    }

    if (tok.type == TOKEN_IMPORT) {
        if (parse_scope_depth > 0)
            diag_emit(stmt_loc, DIAG_ERROR, "import statements must be at the top level");
        return parse_import_stmt(lexer, stmt_loc);
    }

    if (tok.type == TOKEN_LBRACE) {
        ASTNode *node = malloc(sizeof(ASTNode));
        memset(node, 0, sizeof(ASTNode));
        node->type = NODE_BLOCK;
        node->loc = stmt_loc;
        ASTNode *body_head = NULL, *body_tail = NULL;
        for (;;) {
            Token peek = lexer_peek(lexer);
            if (peek.type == TOKEN_RBRACE) { lexer_next(lexer); break; }
            if (peek.type == TOKEN_EOF)
                diag_emit((SourceLoc){peek.line, peek.col}, DIAG_ERROR, "unexpected end of file in block");
            Token stmt_tok = lexer_next(lexer);
            ASTNode *stmt = parse_statement(lexer, stmt_tok);
            if (stmt) {
                if (body_tail) { body_tail->next = stmt; } else { body_head = stmt; }
                body_tail = stmt;
            }
        }
        node->body = body_head;
        return node;
    }

    int is_const = (tok.type == TOKEN_IDENT && tok.length == 5 &&
                    memcmp(tok.start, "const", 5) == 0);
    int is_var   = (tok.type == TOKEN_IDENT && tok.length == 3 &&
                    memcmp(tok.start, "var", 3) == 0);
    int is_fn    = (tok.type == TOKEN_IDENT && tok.length == 2 &&
                    memcmp(tok.start, "fn", 2) == 0);
    int is_return = (tok.type == TOKEN_IDENT && tok.length == 6 &&
                     memcmp(tok.start, "return", 6) == 0);
    int is_class = (tok.type == TOKEN_IDENT && tok.length == 5 &&
                    memcmp(tok.start, "class", 5) == 0);
    int is_enum  = (tok.type == TOKEN_IDENT && tok.length == 4 &&
                    memcmp(tok.start, "enum", 4) == 0);

    if (tok.type == TOKEN_BREAK) {
        if (parse_loop_depth <= 0)
            diag_emit(stmt_loc, DIAG_ERROR, "'break' outside of loop");
        ASTNode *node = malloc(sizeof(ASTNode));
        memset(node, 0, sizeof(ASTNode));
        node->type = NODE_BREAK;
        node->loc = stmt_loc;
        expect(lexer, TOKEN_SEMICOLON, "';'");
        return node;
    }

    if (tok.type == TOKEN_CONTINUE) {
        if (parse_loop_depth <= 0)
            diag_emit(stmt_loc, DIAG_ERROR, "'continue' outside of loop");
        ASTNode *node = malloc(sizeof(ASTNode));
        memset(node, 0, sizeof(ASTNode));
        node->type = NODE_CONTINUE;
        node->loc = stmt_loc;
        expect(lexer, TOKEN_SEMICOLON, "';'");
        return node;
    }

    if (tok.type == TOKEN_SPAWN) {
        ASTNode *node = malloc(sizeof(ASTNode));
        memset(node, 0, sizeof(ASTNode));
        node->type = NODE_SPAWN;
        node->loc = stmt_loc;
        node->spawn_expr = parse_expr(lexer);
        if (node->spawn_expr->kind != EXPR_FN_CALL)
            diag_emit(stmt_loc, DIAG_ERROR, "spawn requires a function call");
        expect(lexer, TOKEN_SEMICOLON, "';'");
        return node;
    }

    if (tok.type == TOKEN_FOR) {
        return parse_for_loop(lexer, stmt_loc);
    }

    if (tok.type == TOKEN_IF) {
        return parse_if_stmt(lexer, stmt_loc);
    }

    if (tok.type == TOKEN_MATCH) {
        return parse_match_stmt(lexer, stmt_loc);
    }

    if (is_class) {
        return parse_class_decl(lexer, stmt_loc);
    }

    if (is_enum) {
        return parse_enum_decl(lexer, stmt_loc);
    }

    if (is_fn) {
        return parse_fn_decl(lexer, stmt_loc);
    }

    if (is_return) {
        ASTNode *node = malloc(sizeof(ASTNode));
        memset(node, 0, sizeof(ASTNode));
        node->type = NODE_RETURN;
        node->loc = stmt_loc;

        if (!try_parse_new_only(lexer, node)) {
            node->expr = parse_expr(lexer);
        }

        expect(lexer, TOKEN_SEMICOLON, "';'");
        return node;
    }

    if (is_const || is_var) {
        Token name = expect(lexer, TOKEN_IDENT, "variable name");

        int has_annotation = 0;
        ValueType annotated_type = VAL_STRING;
        ValueType annotated_array_elem_type = VAL_VOID;

        Token after_name = lexer_next(lexer);
        if (after_name.type == TOKEN_COLON) {
            Token type_tok = expect(lexer, TOKEN_IDENT, "type name");
            has_annotation = 1;
            annotated_type = parse_full_type(lexer, &type_tok, NULL, &annotated_array_elem_type);
            expect(lexer, TOKEN_EQUALS, "'='");
        } else if (after_name.type != TOKEN_EQUALS) {
            diag_emit((SourceLoc){after_name.line, after_name.col}, DIAG_ERROR, "expected ':' or '='");
        }

        ASTNode *node = malloc(sizeof(ASTNode));
        memset(node, 0, sizeof(ASTNode));
        node->type = NODE_VAR_DECL;
        node->loc = stmt_loc;
        node->is_const = is_const;
        node->var_array_elem_type = annotated_array_elem_type;

        if (!try_parse_new_only(lexer, node)) {
            node->expr = parse_expr(lexer);
        }

        node->var_name = malloc(name.length + 1);
        memcpy(node->var_name, name.start, name.length);
        node->var_name[name.length] = '\0';

        /* Type checking for annotations on non-fn-call expressions */
        if (has_annotation && !node->is_fn_call && !node->is_new_expr && node->expr) {
            /* For simple literals, check immediately (skip arrays — checked at codegen) */
            if (annotated_type != VAL_ARRAY &&
                node->expr->kind != EXPR_BINARY && node->expr->kind != EXPR_VAR_REF
                && node->expr->kind != EXPR_MEMBER_ACCESS && node->expr->kind != EXPR_ARRAY_LIT) {
                if (annotated_type != node->expr->value_type) {
                    diag_emit(node->expr->loc, DIAG_ERROR,
                              "type mismatch: variable '%s' declared as '%s', but assigned '%s'",
                              node->var_name, value_type_name(annotated_type), value_type_name(node->expr->value_type));
                }
            }
            /* Store the annotated type for codegen to check */
            node->expr->value_type = annotated_type;
        }

        expect(lexer, TOKEN_SEMICOLON, "';'");
        return node;
    }

    if (tok.type == TOKEN_IDENT && tok.length == 5 &&
        memcmp(tok.start, "print", 5) == 0) {
        expect(lexer, TOKEN_LPAREN, "'('");

        ASTNode *node = malloc(sizeof(ASTNode));
        memset(node, 0, sizeof(ASTNode));
        node->type = NODE_PRINT;
        node->loc = stmt_loc;
        node->print_newline = 1; /* default: append \n */

        if (!try_parse_new_only(lexer, node)) {
            node->expr = parse_expr(lexer);
        }

        /* Check for optional named parameter: newline: false */
        Token peek = lexer_peek(lexer);
        if (peek.type == TOKEN_COMMA) {
            lexer_next(lexer); /* consume ',' */
            Token name_tok = lexer_next(lexer);
            if (name_tok.type != TOKEN_IDENT || name_tok.length != 7 ||
                memcmp(name_tok.start, "newline", 7) != 0) {
                diag_emit((SourceLoc){name_tok.line, name_tok.col}, DIAG_ERROR,
                          "print only accepts the named parameter 'newline'");
            }
            expect(lexer, TOKEN_COLON, "':'");
            Token val_tok = lexer_next(lexer);
            if (val_tok.type != TOKEN_BOOL) {
                diag_emit((SourceLoc){val_tok.line, val_tok.col}, DIAG_ERROR,
                          "'newline' parameter must be a bool (true or false)");
            }
            if (val_tok.length == 5 && memcmp(val_tok.start, "false", 5) == 0) {
                node->print_newline = 0;
            } else {
                node->print_newline = 1;
            }
        }

        expect(lexer, TOKEN_RPAREN, "')'");
        expect(lexer, TOKEN_SEMICOLON, "';'");
        return node;
    }

    if (tok.type == TOKEN_IDENT) {
        Token peek = lexer_peek(lexer);

        /* obj.method(args); or obj.field = value; */
        if (peek.type == TOKEN_DOT) {
            lexer_next(lexer); /* consume '.' */
            Token member = expect(lexer, TOKEN_IDENT, "member name");
            Token after_member = lexer_peek(lexer);

            if (after_member.type == TOKEN_LPAREN) {
                /* Method call: obj.method(args); */
                lexer_next(lexer); /* consume '(' */
                ASTNode *node = malloc(sizeof(ASTNode));
                memset(node, 0, sizeof(ASTNode));
                node->type = NODE_FN_CALL;
                node->loc = stmt_loc;
                node->is_fn_call = 1;
                node->obj_name = malloc(tok.length + 1);
                memcpy(node->obj_name, tok.start, tok.length);
                node->obj_name[tok.length] = '\0';
                node->fn_name = malloc(member.length + 1);
                memcpy(node->fn_name, member.start, member.length);
                node->fn_name[member.length] = '\0';
                parse_call_args(lexer, node);
                expect(lexer, TOKEN_SEMICOLON, "';'");
                return node;
            }

            if (after_member.type == TOKEN_EQUALS) {
                /* Field assignment: obj.field = value; */
                lexer_next(lexer); /* consume '=' */
                ASTNode *node = malloc(sizeof(ASTNode));
                memset(node, 0, sizeof(ASTNode));
                node->type = NODE_ASSIGN;
                node->loc = stmt_loc;
                node->var_name = malloc(tok.length + 1);
                memcpy(node->var_name, tok.start, tok.length);
                node->var_name[tok.length] = '\0';
                node->field_name = malloc(member.length + 1);
                memcpy(node->field_name, member.start, member.length);
                node->field_name[member.length] = '\0';

                if (!try_parse_new_only(lexer, node)) {
                    node->expr = parse_expr(lexer);
                }

                expect(lexer, TOKEN_SEMICOLON, "';'");
                return node;
            }

            diag_emit((SourceLoc){after_member.line, after_member.col}, DIAG_ERROR,
                      "expected '(' or '=' after member name");
        }

        if (peek.type == TOKEN_LPAREN) {
            /* Standalone function call: name(args); */
            lexer_next(lexer); /* consume '(' */

            ASTNode *node = malloc(sizeof(ASTNode));
            memset(node, 0, sizeof(ASTNode));
            node->type = NODE_FN_CALL;
            node->loc = stmt_loc;
            node->is_fn_call = 1;
            node->fn_name = malloc(tok.length + 1);
            memcpy(node->fn_name, tok.start, tok.length);
            node->fn_name[tok.length] = '\0';
            parse_call_args(lexer, node);
            expect(lexer, TOKEN_SEMICOLON, "';'");
            return node;
        }

        /* i++ or i-- */
        if (peek.type == TOKEN_PLUS_PLUS || peek.type == TOKEN_MINUS_MINUS) {
            lexer_next(lexer); /* consume ++ or -- */
            ASTNode *node = malloc(sizeof(ASTNode));
            memset(node, 0, sizeof(ASTNode));
            node->type = NODE_ASSIGN;
            node->loc = stmt_loc;
            node->var_name = malloc(tok.length + 1);
            memcpy(node->var_name, tok.start, tok.length);
            node->var_name[tok.length] = '\0';
            Expr *var = expr_alloc(EXPR_VAR_REF);
            var->loc = stmt_loc;
            var->as.var_ref.name = malloc(tok.length + 1);
            memcpy(var->as.var_ref.name, tok.start, tok.length);
            var->as.var_ref.name[tok.length] = '\0';
            Expr *one = expr_alloc(EXPR_INT_LIT);
            one->loc = stmt_loc;
            one->value_type = VAL_INT;
            one->as.int_lit.value = 1;
            Expr *bin = expr_alloc(EXPR_BINARY);
            bin->loc = stmt_loc;
            bin->as.binary.op = (peek.type == TOKEN_PLUS_PLUS) ? BINOP_ADD : BINOP_SUB;
            bin->as.binary.left = var;
            bin->as.binary.right = one;
            node->expr = bin;
            expect(lexer, TOKEN_SEMICOLON, "';'");
            return node;
        }

        /* Compound assignment: +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>= */
        {
            BinOpKind compound_op;
            int is_compound = 1;
            switch (peek.type) {
                case TOKEN_PLUS_EQ:    compound_op = BINOP_ADD; break;
                case TOKEN_MINUS_EQ:   compound_op = BINOP_SUB; break;
                case TOKEN_STAR_EQ:    compound_op = BINOP_MUL; break;
                case TOKEN_SLASH_EQ:   compound_op = BINOP_DIV; break;
                case TOKEN_PERCENT_EQ: compound_op = BINOP_MOD; break;
                case TOKEN_AMP_EQ:     compound_op = BINOP_BIT_AND; break;
                case TOKEN_PIPE_EQ:    compound_op = BINOP_BIT_OR; break;
                case TOKEN_CARET_EQ:   compound_op = BINOP_BIT_XOR; break;
                case TOKEN_SHL_EQ:     compound_op = BINOP_SHL; break;
                case TOKEN_SHR_EQ:     compound_op = BINOP_SHR; break;
                default: is_compound = 0; break;
            }
            if (is_compound) {
                lexer_next(lexer); /* consume compound operator */
                ASTNode *node = malloc(sizeof(ASTNode));
                memset(node, 0, sizeof(ASTNode));
                node->type = NODE_ASSIGN;
                node->loc = stmt_loc;
                node->var_name = malloc(tok.length + 1);
                memcpy(node->var_name, tok.start, tok.length);
                node->var_name[tok.length] = '\0';
                Expr *var = expr_alloc(EXPR_VAR_REF);
                var->loc = stmt_loc;
                var->as.var_ref.name = malloc(tok.length + 1);
                memcpy(var->as.var_ref.name, tok.start, tok.length);
                var->as.var_ref.name[tok.length] = '\0';
                Expr *rhs = parse_expr(lexer);
                Expr *bin = expr_alloc(EXPR_BINARY);
                bin->loc = stmt_loc;
                bin->as.binary.op = compound_op;
                bin->as.binary.left = var;
                bin->as.binary.right = rhs;
                node->expr = bin;
                expect(lexer, TOKEN_SEMICOLON, "';'");
                return node;
            }
        }

        /* Assignment: <ident> = <value> ; */
        expect(lexer, TOKEN_EQUALS, "'='");

        ASTNode *node = malloc(sizeof(ASTNode));
        memset(node, 0, sizeof(ASTNode));
        node->type = NODE_ASSIGN;
        node->loc = stmt_loc;

        if (!try_parse_new_only(lexer, node)) {
            node->expr = parse_expr(lexer);
        }

        node->var_name = malloc(tok.length + 1);
        memcpy(node->var_name, tok.start, tok.length);
        node->var_name[tok.length] = '\0';
        expect(lexer, TOKEN_SEMICOLON, "';'");
        return node;
    }

    diag_emit(stmt_loc, DIAG_ERROR, "unexpected token");
    return NULL; /* unreachable */
}

ASTNode *parse(Lexer *lexer) {
    ASTNode *head = NULL;
    ASTNode *tail = NULL;

    for (;;) {
        Token tok = lexer_next(lexer);
        if (tok.type == TOKEN_EOF)
            break;

        ASTNode *node = parse_statement(lexer, tok);
        if (node) {
            if (tail) {
                tail->next = node;
            } else {
                head = node;
            }
            tail = node;
        }
    }

    return head;
}

void ast_free(ASTNode *node) {
    while (node) {
        ASTNode *next = node->next;
        free(node->var_name);
        expr_free(node->expr);
        free(node->fn_name);
        if (node->params) {
            for (int i = 0; i < node->param_count; i++) {
                free(node->params[i].name);
                free(node->params[i].default_value);
                free(node->params[i].class_type_name);
            }
            free(node->params);
        }
        if (node->for_init)
            ast_free(node->for_init);
        expr_free(node->for_cond);
        if (node->for_update)
            ast_free(node->for_update);
        if (node->body)
            ast_free(node->body);
        expr_free(node->if_cond);
        if (node->if_body)
            ast_free(node->if_body);
        if (node->else_body)
            ast_free(node->else_body);
        expr_free(node->match_expr);
        if (node->match_arms) {
            for (int i = 0; i < node->match_arm_count; i++) {
                expr_free(node->match_arms[i].pattern);
                ast_free(node->match_arms[i].body);
            }
            free(node->match_arms);
        }
        if (node->call_args) {
            for (int i = 0; i < node->call_arg_count; i++)
                free(node->call_args[i]);
            free(node->call_args);
        }
        free(node->call_arg_types);
        free(node->call_arg_is_var_ref);
        if (node->call_arg_names) {
            for (int i = 0; i < node->call_arg_count; i++)
                free(node->call_arg_names[i]);
            free(node->call_arg_names);
        }
        if (node->call_arg_exprs) {
            for (int i = 0; i < node->call_arg_count; i++)
                expr_free(node->call_arg_exprs[i]);
            free(node->call_arg_exprs);
        }
        free(node->class_name);
        free(node->parent_class_name);
        if (node->class_fields) {
            for (int i = 0; i < node->class_field_count; i++) {
                free(node->class_fields[i].name);
                free(node->class_fields[i].class_type_name);
            }
            free(node->class_fields);
        }
        if (node->class_methods)
            ast_free(node->class_methods);
        free(node->obj_name);
        free(node->field_name);
        free(node->enum_name);
        if (node->enum_variants) {
            for (int i = 0; i < node->enum_variant_count; i++)
                free(node->enum_variants[i].name);
            free(node->enum_variants);
        }
        expr_free(node->spawn_expr);
        free(node->import_path);
        if (node->import_names) {
            for (int i = 0; i < node->import_name_count; i++)
                free(node->import_names[i]);
            free(node->import_names);
        }
        free(node);
        node = next;
    }
}
