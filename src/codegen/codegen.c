#define _POSIX_C_SOURCE 200809L
#include "codegen.h"
#include "codegen/codegen_internal.h"
#include "diagnostic.h"
#include "import.h"
#include <libgen.h>
#include <string.h>

/* If no target backend matched, fail at link time with a clear message. */
#if !defined(__linux__) || !defined(__x86_64__)
#if !defined(__APPLE__) || !defined(__aarch64__)
/* Provide a stub so the compiler still reports the real problem. */
int emit_binary(int string_count, int *str_offsets, int *str_lengths_arr,
                Buffer *strings, const char *output_path)
{
    (void)string_count; (void)str_offsets; (void)str_lengths_arr;
    (void)strings; (void)output_path;
    diag_error_no_loc("no codegen backend for this platform");
    return 1;
}
int emit_http_binary(HttpRouteEntry *routes, int route_count, int port,
                     const char *output_path)
{
    (void)routes; (void)route_count; (void)port; (void)output_path;
    diag_error_no_loc("no codegen backend for this platform");
    return 1;
}
int emit_net_binary(NetConfig *config, const char *output_path)
{
    (void)config; (void)output_path;
    diag_error_no_loc("no codegen backend for this platform");
    return 1;
}
int emit_binary_ir(IRProgram *prog, const char *output_path)
{
    (void)prog; (void)output_path;
    diag_error_no_loc("no codegen backend for this platform");
    return 1;
}
#endif
#endif

/* ================================================================
 * Function table
 * ================================================================ */

typedef struct {
    char *name;
    ASTNode *decl;
} FnEntry;

typedef struct {
    FnEntry *entries;
    int count;
    int cap;
    char **evaluating;
    int eval_count;
    int eval_cap;
} FnTable;

static void fn_table_init(FnTable *ft) {
    ft->cap = 8;
    ft->count = 0;
    ft->entries = malloc(ft->cap * sizeof(FnEntry));
    ft->eval_cap = 8;
    ft->eval_count = 0;
    ft->evaluating = malloc(ft->eval_cap * sizeof(char *));
}

static void fn_table_free(FnTable *ft) {
    free(ft->entries);
    free(ft->evaluating);
}

static FnEntry *fn_table_find(FnTable *ft, const char *name) {
    for (int i = 0; i < ft->count; i++)
        if (strcmp(name, ft->entries[i].name) == 0)
            return &ft->entries[i];
    return NULL;
}

static void fn_table_add(FnTable *ft, char *name, ASTNode *decl) {
    if (ft->count == ft->cap) {
        ft->cap *= 2;
        ft->entries = realloc(ft->entries, ft->cap * sizeof(FnEntry));
    }
    ft->entries[ft->count].name = name;
    ft->entries[ft->count].decl = decl;
    ft->count++;
}

/* ================================================================
 * Dynamic print list
 * ================================================================ */

typedef struct {
    char **strings;
    int *lengths;
    int count;
    int cap;
} PrintList;

static void print_list_init(PrintList *pl) {
    pl->cap = 16;
    pl->count = 0;
    pl->strings = malloc(pl->cap * sizeof(char *));
    pl->lengths = malloc(pl->cap * sizeof(int));
}

static void print_list_free(PrintList *pl) {
    free(pl->strings);
    free(pl->lengths);
}

static void print_list_add(PrintList *pl, const char *str, int len) {
    if (pl->count == pl->cap) {
        pl->cap *= 2;
        pl->strings = realloc(pl->strings, pl->cap * sizeof(char *));
        pl->lengths = realloc(pl->lengths, pl->cap * sizeof(int));
    }
    pl->strings[pl->count] = (char *)str;
    pl->lengths[pl->count] = len;
    pl->count++;
}

/* ================================================================
 * ObjData — compile-time object instance
 * ================================================================ */

typedef struct {
    char *class_name;
    char **field_names;
    int field_count;
    /* field_values is an array of EvalResult (forward declared below) */
    struct EvalResultS *field_values;
} ObjData;

/* ================================================================
 * ArrayData — compile-time array instance (forward declared)
 * ================================================================ */

typedef struct ArrayDataS ArrayData;
typedef struct ChannelDataS ChannelData;

/* ================================================================
 * EvalResult — compile-time evaluated value
 * ================================================================ */

typedef struct EvalResultS {
    ValueType type;
    long int_val;
    double float_val;
    char *str_val;
    int str_len;
    int bool_val;
    ObjData *obj_val;
    ArrayData *arr_val;
    ChannelData *chan_val;
} EvalResult;

struct ArrayDataS {
    EvalResult *elements;
    int count;
    ValueType elem_type;
};

struct ChannelDataS {
    EvalResult *buffer;
    int count;
    int cap;
    int read_pos;
    ValueType elem_type;
};

/* ================================================================
 * ClassDef — compile-time class definition
 * ================================================================ */

typedef struct {
    char *name;
    char *parent_name;
    char **field_names;
    ValueType *field_types;
    int field_count;
    ASTNode *methods; /* linked list of NODE_FN_DECL */
    SourceLoc loc;
} ClassDef;

typedef struct {
    ClassDef *entries;
    int count;
    int cap;
} ClassTable;

static void class_table_init(ClassTable *ct) {
    ct->cap = 8;
    ct->count = 0;
    ct->entries = malloc(ct->cap * sizeof(ClassDef));
}

static void class_table_free(ClassTable *ct) {
    for (int i = 0; i < ct->count; i++) {
        free(ct->entries[i].field_names);
        free(ct->entries[i].field_types);
    }
    free(ct->entries);
}

static ClassDef *class_table_find(ClassTable *ct, const char *name) {
    for (int i = 0; i < ct->count; i++)
        if (strcmp(name, ct->entries[i].name) == 0)
            return &ct->entries[i];
    return NULL;
}


/* ================================================================
 * EnumDef — compile-time enum definition
 * ================================================================ */

typedef struct {
    char *name;
    char **variant_names;
    long *variant_values;
    int variant_count;
    SourceLoc loc;
} EnumDef;

typedef struct {
    EnumDef *entries;
    int count;
    int cap;
} EnumTable;

static void enum_table_init(EnumTable *et) {
    et->cap = 8;
    et->count = 0;
    et->entries = malloc(et->cap * sizeof(EnumDef));
}

static void enum_table_free(EnumTable *et) {
    free(et->entries);
}

static EnumDef *enum_table_find(EnumTable *et, const char *name) {
    for (int i = 0; i < et->count; i++)
        if (strcmp(name, et->entries[i].name) == 0)
            return &et->entries[i];
    return NULL;
}

/* ================================================================
 * Symbol table with parent chain for block scoping
 * ================================================================ */

typedef struct {
    char *name;
    EvalResult val;
    int is_const;
    int mutated;
    SourceLoc loc;
    int has_slot;       /* 1 if this variable has a runtime stack slot */
    int slot;           /* IR slot number (valid when has_slot == 1) */
} Symbol;

typedef struct SymTable {
    Symbol *syms;
    int count;
    int cap;
    struct SymTable *parent;
} SymTable;

static void sym_table_init(SymTable *st) {
    st->cap = 16;
    st->count = 0;
    st->syms = malloc(st->cap * sizeof(Symbol));
    st->parent = NULL;
}

static void sym_table_free(SymTable *st) {
    free(st->syms);
}

/* Lookup in current scope only */
static int sym_lookup(SymTable *st, const char *name) {
    for (int i = 0; i < st->count; i++)
        if (strcmp(name, st->syms[i].name) == 0) return i;
    return -1;
}

/* Walk parent chain to find a symbol */
static Symbol *sym_find(SymTable *st, const char *name) {
    for (SymTable *s = st; s; s = s->parent) {
        int idx = sym_lookup(s, name);
        if (idx >= 0) return &s->syms[idx];
    }
    return NULL;
}

static void sym_add(SymTable *st, const char *name, EvalResult val, int is_const, SourceLoc loc) {
    if (st->count == st->cap) {
        st->cap *= 2;
        st->syms = realloc(st->syms, st->cap * sizeof(Symbol));
    }
    st->syms[st->count].name = (char *)name;
    st->syms[st->count].val = val;
    st->syms[st->count].is_const = is_const;
    st->syms[st->count].mutated = 0;
    st->syms[st->count].loc = loc;
    st->syms[st->count].has_slot = 0;
    st->syms[st->count].slot = -1;
    st->count++;
}

/* ================================================================
 * Expression evaluation (single SymTable with parent chain)
 * ================================================================ */

/* Global codegen context for use by eval_expr (EXPR_FN_CALL) */
static FnTable *g_ft;
static ClassTable *g_ct;
static EnumTable *g_et;
static PrintList *g_prints;

/* ================================================================
 * Standard library import tracking
 * ================================================================ */

static const char *g_stdlib_string_fns[] = {
    "len", "trim", "contains", "replace", "to_upper", "to_lower",
    "starts_with", "ends_with", "index_of", "char_at", "substr"
};
#define STDLIB_STRING_FN_COUNT 11
static char g_stdlib_imported_flags[STDLIB_STRING_FN_COUNT];

static const char *g_stdlib_array_fns[] = {
    "push", "pop", "shift", "concat", "reverse", "sort", "join", "remove"
};
#define STDLIB_ARRAY_FN_COUNT 8
static char g_stdlib_array_imported_flags[STDLIB_ARRAY_FN_COUNT];

static const char *g_stdlib_concurrency_fns[] = { "send", "receive" };
#define STDLIB_CONCURRENCY_FN_COUNT 2
static char g_stdlib_concurrency_imported_flags[STDLIB_CONCURRENCY_FN_COUNT];

/* HTTP stdlib */
static const char *g_stdlib_http_fns[] = { "get", "post", "listen" };
#define STDLIB_HTTP_FN_COUNT 3
static char g_stdlib_http_imported_flags[STDLIB_HTTP_FN_COUNT];

/* HTTP route table */
static HttpRouteEntry *g_http_routes = NULL;
static int g_http_route_count = 0;
static int g_http_route_cap = 0;
static int g_http_listen_port = 0;
static char g_http_listen_called = 0;

/* Net stdlib */
static const char *g_stdlib_net_fns[] = {
    "tcp_listen", "tcp_connect", "udp_listen", "udp_send", "start"
};
#define STDLIB_NET_FN_COUNT 5
static char g_stdlib_net_imported_flags[STDLIB_NET_FN_COUNT];

/* Net config */
static NetConfig g_net_config;
static char g_net_mode_set = 0;
static char g_net_start_called = 0;

static void stdlib_reset(void) {
    memset(g_stdlib_imported_flags, 0, sizeof(g_stdlib_imported_flags));
    memset(g_stdlib_array_imported_flags, 0, sizeof(g_stdlib_array_imported_flags));
    memset(g_stdlib_concurrency_imported_flags, 0, sizeof(g_stdlib_concurrency_imported_flags));
    memset(g_stdlib_http_imported_flags, 0, sizeof(g_stdlib_http_imported_flags));
    g_http_routes = NULL;
    g_http_route_count = 0;
    g_http_route_cap = 0;
    g_http_listen_port = 0;
    g_http_listen_called = 0;
    memset(g_stdlib_net_imported_flags, 0, sizeof(g_stdlib_net_imported_flags));
    memset(&g_net_config, 0, sizeof(g_net_config));
    g_net_mode_set = 0;
    g_net_start_called = 0;
}

static int stdlib_fn_index(const char *name) {
    for (int i = 0; i < STDLIB_STRING_FN_COUNT; i++)
        if (strcmp(g_stdlib_string_fns[i], name) == 0) return i;
    return -1;
}

static int stdlib_fn_is_imported(const char *name) {
    int idx = stdlib_fn_index(name);
    return idx >= 0 && g_stdlib_imported_flags[idx];
}

static void stdlib_fn_import(const char *name) {
    int idx = stdlib_fn_index(name);
    if (idx >= 0) g_stdlib_imported_flags[idx] = 1;
}

static int stdlib_array_fn_index(const char *name) {
    for (int i = 0; i < STDLIB_ARRAY_FN_COUNT; i++)
        if (strcmp(g_stdlib_array_fns[i], name) == 0) return i;
    return -1;
}

static int stdlib_array_fn_is_imported(const char *name) {
    int idx = stdlib_array_fn_index(name);
    return idx >= 0 && g_stdlib_array_imported_flags[idx];
}

static void stdlib_array_fn_import(const char *name) {
    int idx = stdlib_array_fn_index(name);
    if (idx >= 0) g_stdlib_array_imported_flags[idx] = 1;
}

static int stdlib_concurrency_fn_index(const char *name) {
    for (int i = 0; i < STDLIB_CONCURRENCY_FN_COUNT; i++)
        if (strcmp(g_stdlib_concurrency_fns[i], name) == 0) return i;
    return -1;
}

static int stdlib_concurrency_fn_is_imported(const char *name) {
    int idx = stdlib_concurrency_fn_index(name);
    return idx >= 0 && g_stdlib_concurrency_imported_flags[idx];
}

static void stdlib_concurrency_fn_import(const char *name) {
    int idx = stdlib_concurrency_fn_index(name);
    if (idx >= 0) g_stdlib_concurrency_imported_flags[idx] = 1;
}

static int stdlib_http_fn_index(const char *name) {
    for (int i = 0; i < STDLIB_HTTP_FN_COUNT; i++)
        if (strcmp(g_stdlib_http_fns[i], name) == 0) return i;
    return -1;
}

static int stdlib_http_fn_is_imported(const char *name) {
    int idx = stdlib_http_fn_index(name);
    return idx >= 0 && g_stdlib_http_imported_flags[idx];
}

static void stdlib_http_fn_import(const char *name) {
    int idx = stdlib_http_fn_index(name);
    if (idx >= 0) g_stdlib_http_imported_flags[idx] = 1;
}

static int stdlib_net_fn_index(const char *name) {
    for (int i = 0; i < STDLIB_NET_FN_COUNT; i++)
        if (strcmp(g_stdlib_net_fns[i], name) == 0) return i;
    return -1;
}

static int stdlib_net_fn_is_imported(const char *name) {
    int idx = stdlib_net_fn_index(name);
    return idx >= 0 && g_stdlib_net_imported_flags[idx];
}

static void stdlib_net_fn_import(const char *name) {
    int idx = stdlib_net_fn_index(name);
    if (idx >= 0) g_stdlib_net_imported_flags[idx] = 1;
}

/* ================================================================
 * IR compilation support — globals and helpers
 * ================================================================ */

static IRProgram *g_ir = NULL;       /* NULL when IR not active */
static int g_ir_mode = 0;           /* 1 once any IR print has been emitted */

/* Check if an expression involves runtime variables (has_slot symbols) */
static int expr_is_runtime(Expr *expr, SymTable *st) {
    if (!expr) return 0;
    switch (expr->kind) {
    case EXPR_VAR_REF: {
        Symbol *sym = sym_find(st, expr->as.var_ref.name);
        return sym && sym->has_slot;
    }
    case EXPR_BINARY:
        return expr_is_runtime(expr->as.binary.left, st) ||
               expr_is_runtime(expr->as.binary.right, st);
    case EXPR_UNARY:
        return expr_is_runtime(expr->as.unary.operand, st);
    case EXPR_INT_LIT:
    case EXPR_FLOAT_LIT:
    case EXPR_STRING_LIT:
    case EXPR_BOOL_LIT:
        return 0;
    case EXPR_INDEX:
        return expr_is_runtime(expr->as.index_access.object, st) ||
               expr_is_runtime(expr->as.index_access.index, st);
    case EXPR_FN_CALL: {
        for (int i = 0; i < expr->as.fn_call.arg_count; i++) {
            if (expr_is_runtime(expr->as.fn_call.args[i], st))
                return 1;
        }
        return 0;
    }
    default:
        return 0;
    }
}

/* Forward declare eval_expr (needed by ir_compile_expr for const-folding) */
static EvalResult eval_expr(Expr *expr, SymTable *st);

/* Compile an int expression to IR instructions, returns vreg holding result */
static int ir_compile_expr(Expr *expr, SymTable *st, IRProgram *prog) {
    switch (expr->kind) {
    case EXPR_INT_LIT:
        return ir_emit_const_int(prog, expr->as.int_lit.value);

    case EXPR_BOOL_LIT:
        return ir_emit_const_int(prog, expr->as.bool_lit.value);

    case EXPR_VAR_REF: {
        Symbol *sym = sym_find(st, expr->as.var_ref.name);
        if (sym && sym->has_slot) {
            return ir_emit_load(prog, sym->slot);
        }
        /* Compile-time constant — fold its value */
        int64_t cv = (sym->val.type == VAL_BOOL) ? (int64_t)sym->val.bool_val : sym->val.int_val;
        return ir_emit_const_int(prog, cv);
    }

    case EXPR_BINARY: {
        int lhs = ir_compile_expr(expr->as.binary.left, st, prog);
        int rhs = ir_compile_expr(expr->as.binary.right, st, prog);
        IROpcode op;
        switch (expr->as.binary.op) {
        case BINOP_ADD: op = IR_ADD; break;
        case BINOP_SUB: op = IR_SUB; break;
        case BINOP_MUL: op = IR_MUL; break;
        case BINOP_DIV: op = IR_DIV; break;
        case BINOP_MOD: op = IR_MOD; break;
        case BINOP_EQ:  op = IR_CMP_EQ; break;
        case BINOP_NE:  op = IR_CMP_NE; break;
        case BINOP_LT:  op = IR_CMP_LT; break;
        case BINOP_LE:  op = IR_CMP_LE; break;
        case BINOP_GT:  op = IR_CMP_GT; break;
        case BINOP_GE:  op = IR_CMP_GE; break;
        case BINOP_BIT_AND: op = IR_BIT_AND; break;
        case BINOP_BIT_OR:  op = IR_BIT_OR; break;
        case BINOP_BIT_XOR: op = IR_BIT_XOR; break;
        case BINOP_SHL: op = IR_SHL; break;
        case BINOP_SHR: op = IR_SHR; break;
        case BINOP_AND: op = IR_BIT_AND; break; /* logical and on ints (0/1) */
        case BINOP_OR:  op = IR_BIT_OR; break;  /* logical or on ints (0/1) */
        default: op = IR_ADD; break;
        }
        return ir_emit_binop(prog, op, lhs, rhs);
    }

    case EXPR_UNARY: {
        int operand = ir_compile_expr(expr->as.unary.operand, st, prog);
        if (expr->as.unary.op == UNOP_NEG) {
            IRInstr instr;
            memset(&instr, 0, sizeof(instr));
            instr.op = IR_NEG;
            instr.dst = ir_alloc_vreg(prog);
            instr.src = operand;
            ir_emit(prog, instr);
            return instr.dst;
        } else {
            /* UNOP_BIT_NOT */
            IRInstr instr;
            memset(&instr, 0, sizeof(instr));
            instr.op = IR_BIT_NOT;
            instr.dst = ir_alloc_vreg(prog);
            instr.src = operand;
            ir_emit(prog, instr);
            return instr.dst;
        }
    }

    default:
        /* Fallback: evaluate at compile time and emit as constant */
        {
            EvalResult r = eval_expr(expr, st);
            if (r.type == VAL_INT)
                return ir_emit_const_int(prog, r.int_val);
            else if (r.type == VAL_BOOL)
                return ir_emit_const_int(prog, r.bool_val);
            return ir_emit_const_int(prog, 0);
        }
    }
}

/* Determine the runtime type of an expression (for IR print dispatch).
 * Returns VAL_BOOL if the expression produces a bool, VAL_INT otherwise. */
static ValueType expr_runtime_type(Expr *expr, SymTable *st) {
    if (!expr) return VAL_INT;
    switch (expr->kind) {
    case EXPR_BOOL_LIT:
        return VAL_BOOL;
    case EXPR_VAR_REF: {
        Symbol *sym = sym_find(st, expr->as.var_ref.name);
        if (sym) return sym->val.type;
        return VAL_INT;
    }
    case EXPR_BINARY:
        if (expr->as.binary.op >= BINOP_EQ && expr->as.binary.op <= BINOP_LE)
            return VAL_BOOL;
        if (expr->as.binary.op == BINOP_AND || expr->as.binary.op == BINOP_OR)
            return VAL_BOOL;
        return expr_runtime_type(expr->as.binary.left, st);
    case EXPR_UNARY:
        return expr_runtime_type(expr->as.unary.operand, st);
    default:
        return VAL_INT;
    }
}

static char *eval_to_string(EvalResult *r, int *out_len);
static EvalResult evaluate_fn_call(FnTable *ft, ClassTable *ct, SymTable *outer_st,
                                   const char *fn_name, SourceLoc call_loc,
                                   int arg_count,
                                   EvalResult *arg_results,
                                   char **arg_names,
                                   PrintList *prints);
static EvalResult evaluate_method_call(ASTNode *n, SymTable *st, FnTable *ft,
                                       ClassTable *ct, PrintList *prints,
                                       int require_value);

static EvalResult eval_binary(BinOpKind op, EvalResult lhs, EvalResult rhs, SourceLoc loc) {
    EvalResult r;
    memset(&r, 0, sizeof(r));

    /* Logical operators */
    if (op == BINOP_AND || op == BINOP_OR) {
        if (lhs.type != VAL_BOOL)
            diag_emit(loc, DIAG_ERROR, "left operand of '%s' is not a bool",
                      op == BINOP_AND ? "and" : "or");
        if (rhs.type != VAL_BOOL)
            diag_emit(loc, DIAG_ERROR, "right operand of '%s' is not a bool",
                      op == BINOP_AND ? "and" : "or");
        r.type = VAL_BOOL;
        r.bool_val = (op == BINOP_AND) ? (lhs.bool_val && rhs.bool_val) : (lhs.bool_val || rhs.bool_val);
        return r;
    }

    /* Comparison operators */
    if (op >= BINOP_EQ && op <= BINOP_LE) {
        /* Array comparison (== and != only) */
        if (lhs.type == VAL_ARRAY && rhs.type == VAL_ARRAY) {
            if (op != BINOP_EQ && op != BINOP_NE)
                diag_emit(loc, DIAG_ERROR, "only == and != are supported for arrays");
            ArrayData *la = lhs.arr_val;
            ArrayData *ra = rhs.arr_val;
            int equal = 1;
            if (!la || !ra) {
                equal = (la == ra);
            } else if (la->count != ra->count) {
                equal = 0;
            } else {
                for (int i = 0; i < la->count; i++) {
                    EvalResult cmp = eval_binary(BINOP_EQ, la->elements[i], ra->elements[i], loc);
                    if (!cmp.bool_val) { equal = 0; break; }
                }
            }
            r.type = VAL_BOOL;
            r.bool_val = (op == BINOP_EQ) ? equal : !equal;
            return r;
        }

        /* Channel comparison (== and != only, identity/pointer comparison) */
        if (lhs.type == VAL_CHANNEL && rhs.type == VAL_CHANNEL) {
            if (op != BINOP_EQ && op != BINOP_NE)
                diag_emit(loc, DIAG_ERROR, "only == and != are supported for channels");
            int equal = (lhs.chan_val == rhs.chan_val);
            r.type = VAL_BOOL;
            r.bool_val = (op == BINOP_EQ) ? equal : !equal;
            return r;
        }

        /* Type promotion for comparisons: int op float → float */
        if ((lhs.type == VAL_INT && rhs.type == VAL_FLOAT) ||
            (lhs.type == VAL_FLOAT && rhs.type == VAL_INT)) {
            double a = (lhs.type == VAL_INT) ? (double)lhs.int_val : lhs.float_val;
            double b = (rhs.type == VAL_INT) ? (double)rhs.int_val : rhs.float_val;
            r.type = VAL_BOOL;
            switch (op) {
                case BINOP_EQ: r.bool_val = a == b; break;
                case BINOP_NE: r.bool_val = a != b; break;
                case BINOP_GT: r.bool_val = a > b; break;
                case BINOP_GE: r.bool_val = a >= b; break;
                case BINOP_LT: r.bool_val = a < b; break;
                case BINOP_LE: r.bool_val = a <= b; break;
                default: break;
            }
            return r;
        }

        if (lhs.type != rhs.type)
            diag_emit(loc, DIAG_ERROR, "cannot compare '%s' with '%s'",
                      value_type_name(lhs.type), value_type_name(rhs.type));

        r.type = VAL_BOOL;
        if (lhs.type == VAL_INT) {
            switch (op) {
                case BINOP_EQ: r.bool_val = lhs.int_val == rhs.int_val; break;
                case BINOP_NE: r.bool_val = lhs.int_val != rhs.int_val; break;
                case BINOP_GT: r.bool_val = lhs.int_val > rhs.int_val; break;
                case BINOP_GE: r.bool_val = lhs.int_val >= rhs.int_val; break;
                case BINOP_LT: r.bool_val = lhs.int_val < rhs.int_val; break;
                case BINOP_LE: r.bool_val = lhs.int_val <= rhs.int_val; break;
                default: break;
            }
        } else if (lhs.type == VAL_FLOAT) {
            switch (op) {
                case BINOP_EQ: r.bool_val = lhs.float_val == rhs.float_val; break;
                case BINOP_NE: r.bool_val = lhs.float_val != rhs.float_val; break;
                case BINOP_GT: r.bool_val = lhs.float_val > rhs.float_val; break;
                case BINOP_GE: r.bool_val = lhs.float_val >= rhs.float_val; break;
                case BINOP_LT: r.bool_val = lhs.float_val < rhs.float_val; break;
                case BINOP_LE: r.bool_val = lhs.float_val <= rhs.float_val; break;
                default: break;
            }
        } else if (lhs.type == VAL_STRING) {
            int cmp = strcmp(lhs.str_val, rhs.str_val);
            switch (op) {
                case BINOP_EQ: r.bool_val = cmp == 0; break;
                case BINOP_NE: r.bool_val = cmp != 0; break;
                case BINOP_GT: r.bool_val = cmp > 0; break;
                case BINOP_GE: r.bool_val = cmp >= 0; break;
                case BINOP_LT: r.bool_val = cmp < 0; break;
                case BINOP_LE: r.bool_val = cmp <= 0; break;
                default: break;
            }
        } else if (lhs.type == VAL_BOOL) {
            if (op != BINOP_EQ && op != BINOP_NE)
                diag_emit(loc, DIAG_ERROR, "ordering comparisons not supported for bool");
            r.bool_val = (op == BINOP_EQ) ? (lhs.bool_val == rhs.bool_val) : (lhs.bool_val != rhs.bool_val);
        }
        return r;
    }

    /* Bitwise operators (int-only) */
    if (op == BINOP_BIT_AND || op == BINOP_BIT_OR || op == BINOP_BIT_XOR ||
        op == BINOP_SHL || op == BINOP_SHR) {
        if (lhs.type != VAL_INT)
            diag_emit(loc, DIAG_ERROR, "bitwise operator requires int operands, got '%s'", value_type_name(lhs.type));
        if (rhs.type != VAL_INT)
            diag_emit(loc, DIAG_ERROR, "bitwise operator requires int operands, got '%s'", value_type_name(rhs.type));
        r.type = VAL_INT;
        switch (op) {
            case BINOP_BIT_AND: r.int_val = lhs.int_val & rhs.int_val; break;
            case BINOP_BIT_OR:  r.int_val = lhs.int_val | rhs.int_val; break;
            case BINOP_BIT_XOR: r.int_val = lhs.int_val ^ rhs.int_val; break;
            case BINOP_SHL:     r.int_val = lhs.int_val << rhs.int_val; break;
            case BINOP_SHR:     r.int_val = lhs.int_val >> rhs.int_val; break;
            default: break;
        }
        return r;
    }

    /* Arithmetic operators */

    /* String concatenation with + (auto-convert non-string operand) */
    if (op == BINOP_ADD && (lhs.type == VAL_STRING || rhs.type == VAL_STRING)) {
        /* Auto-convert non-string operand to string */
        if (lhs.type != VAL_STRING) {
            int tmp_len;
            char *tmp = eval_to_string(&lhs, &tmp_len);
            lhs.type = VAL_STRING;
            lhs.str_val = tmp;
            lhs.str_len = tmp_len;
        }
        if (rhs.type != VAL_STRING) {
            int tmp_len;
            char *tmp = eval_to_string(&rhs, &tmp_len);
            rhs.type = VAL_STRING;
            rhs.str_val = tmp;
            rhs.str_len = tmp_len;
        }
        r.type = VAL_STRING;
        r.str_len = lhs.str_len + rhs.str_len;
        r.str_val = malloc(r.str_len + 1);
        memcpy(r.str_val, lhs.str_val, lhs.str_len);
        memcpy(r.str_val + lhs.str_len, rhs.str_val, rhs.str_len);
        r.str_val[r.str_len] = '\0';
        return r;
    }

    /* Type checking for arithmetic */
    if (lhs.type != VAL_INT && lhs.type != VAL_FLOAT)
        diag_emit(loc, DIAG_ERROR, "arithmetic not supported for '%s'", value_type_name(lhs.type));
    if (rhs.type != VAL_INT && rhs.type != VAL_FLOAT)
        diag_emit(loc, DIAG_ERROR, "arithmetic not supported for '%s'", value_type_name(rhs.type));

    /* Modulo: int only */
    if (op == BINOP_MOD) {
        if (lhs.type != VAL_INT || rhs.type != VAL_INT)
            diag_emit(loc, DIAG_ERROR, "'%%' operator requires int operands");
        if (rhs.int_val == 0)
            diag_emit(loc, DIAG_ERROR, "division by zero");
        r.type = VAL_INT;
        r.int_val = lhs.int_val % rhs.int_val;
        return r;
    }

    /* Type promotion: int op float → float */
    if (lhs.type == VAL_FLOAT || rhs.type == VAL_FLOAT) {
        double a = (lhs.type == VAL_INT) ? (double)lhs.int_val : lhs.float_val;
        double b = (rhs.type == VAL_INT) ? (double)rhs.int_val : rhs.float_val;
        r.type = VAL_FLOAT;
        switch (op) {
            case BINOP_ADD: r.float_val = a + b; break;
            case BINOP_SUB: r.float_val = a - b; break;
            case BINOP_MUL: r.float_val = a * b; break;
            case BINOP_DIV:
                if (b == 0.0)
                    diag_emit(loc, DIAG_ERROR, "division by zero");
                r.float_val = a / b;
                break;
            default: break;
        }
        return r;
    }

    /* Both int */
    r.type = VAL_INT;
    switch (op) {
        case BINOP_ADD: r.int_val = lhs.int_val + rhs.int_val; break;
        case BINOP_SUB: r.int_val = lhs.int_val - rhs.int_val; break;
        case BINOP_MUL: r.int_val = lhs.int_val * rhs.int_val; break;
        case BINOP_DIV:
            if (rhs.int_val == 0)
                diag_emit(loc, DIAG_ERROR, "division by zero");
            r.int_val = lhs.int_val / rhs.int_val;
            break;
        default: break;
    }
    return r;
}

static EvalResult eval_expr(Expr *expr, SymTable *st) {
    EvalResult r;
    memset(&r, 0, sizeof(r));

    switch (expr->kind) {
        case EXPR_INT_LIT:
            r.type = VAL_INT;
            r.int_val = expr->as.int_lit.value;
            return r;
        case EXPR_FLOAT_LIT:
            r.type = VAL_FLOAT;
            r.float_val = expr->as.float_lit.value;
            return r;
        case EXPR_STRING_LIT:
            r.type = VAL_STRING;
            r.str_val = expr->as.string_lit.value;
            r.str_len = expr->as.string_lit.len;
            return r;
        case EXPR_BOOL_LIT:
            r.type = VAL_BOOL;
            r.bool_val = expr->as.bool_lit.value;
            return r;
        case EXPR_VAR_REF: {
            Symbol *sym = sym_find(st, expr->as.var_ref.name);
            if (!sym)
                diag_emit(expr->loc, DIAG_ERROR, "undefined variable '%s'", expr->as.var_ref.name);
            return sym->val;
        }
        case EXPR_BINARY: {
            EvalResult lhs = eval_expr(expr->as.binary.left, st);
            EvalResult rhs = eval_expr(expr->as.binary.right, st);
            return eval_binary(expr->as.binary.op, lhs, rhs, expr->loc);
        }
        case EXPR_MEMBER_ACCESS: {
            /* Check for enum access: EnumName.Variant */
            if (expr->as.member_access.object->kind == EXPR_VAR_REF && g_et) {
                const char *enum_name = expr->as.member_access.object->as.var_ref.name;
                EnumDef *edef = enum_table_find(g_et, enum_name);
                if (edef) {
                    const char *variant = expr->as.member_access.field_name;
                    for (int i = 0; i < edef->variant_count; i++) {
                        if (strcmp(edef->variant_names[i], variant) == 0) {
                            r.type = VAL_INT;
                            r.int_val = edef->variant_values[i];
                            return r;
                        }
                    }
                    diag_emit(expr->loc, DIAG_ERROR, "no variant '%s' in enum '%s'",
                              variant, enum_name);
                    return r; /* unreachable */
                }
            }
            EvalResult obj = eval_expr(expr->as.member_access.object, st);
            if (obj.type != VAL_OBJECT || !obj.obj_val)
                diag_emit(expr->loc, DIAG_ERROR, "member access on non-object value");
            const char *fname = expr->as.member_access.field_name;
            for (int i = 0; i < obj.obj_val->field_count; i++) {
                if (strcmp(obj.obj_val->field_names[i], fname) == 0)
                    return obj.obj_val->field_values[i];
            }
            diag_emit(expr->loc, DIAG_ERROR, "no field '%s' on object of class '%s'",
                      fname, obj.obj_val->class_name);
            return r; /* unreachable */
        }
        case EXPR_UNARY: {
            EvalResult operand = eval_expr(expr->as.unary.operand, st);
            if (expr->as.unary.op == UNOP_NEG) {
                if (operand.type == VAL_INT) {
                    r.type = VAL_INT;
                    r.int_val = -operand.int_val;
                } else if (operand.type == VAL_FLOAT) {
                    r.type = VAL_FLOAT;
                    r.float_val = -operand.float_val;
                } else {
                    diag_emit(expr->loc, DIAG_ERROR, "unary '-' requires int or float, got '%s'",
                              value_type_name(operand.type));
                }
            } else if (expr->as.unary.op == UNOP_BIT_NOT) {
                if (operand.type != VAL_INT)
                    diag_emit(expr->loc, DIAG_ERROR, "'~' requires int operand, got '%s'",
                              value_type_name(operand.type));
                r.type = VAL_INT;
                r.int_val = ~operand.int_val;
            }
            return r;
        }
        case EXPR_INDEX: {
            EvalResult obj = eval_expr(expr->as.index_access.object, st);
            EvalResult idx = eval_expr(expr->as.index_access.index, st);
            if (obj.type == VAL_ARRAY) {
                if (idx.type != VAL_INT)
                    diag_emit(expr->loc, DIAG_ERROR, "array index must be an int, got '%s'",
                              value_type_name(idx.type));
                if (!obj.arr_val)
                    diag_emit(expr->loc, DIAG_ERROR, "indexing on null array");
                long i = idx.int_val;
                if (i < 0) i += obj.arr_val->count;
                if (i < 0 || i >= obj.arr_val->count)
                    diag_emit(expr->loc, DIAG_ERROR, "array index %ld out of range (length %d)",
                              idx.int_val, obj.arr_val->count);
                return obj.arr_val->elements[i];
            }
            if (obj.type != VAL_STRING)
                diag_emit(expr->loc, DIAG_ERROR, "indexing requires a string or array, got '%s'",
                          value_type_name(obj.type));
            if (idx.type != VAL_INT)
                diag_emit(expr->loc, DIAG_ERROR, "index must be an int, got '%s'",
                          value_type_name(idx.type));
            long i = idx.int_val;
            if (i < 0) i += obj.str_len;
            if (i < 0 || i >= obj.str_len)
                diag_emit(expr->loc, DIAG_ERROR, "string index %ld out of range (length %d)",
                          idx.int_val, obj.str_len);
            r.type = VAL_STRING;
            r.str_val = malloc(2);
            r.str_val[0] = obj.str_val[i];
            r.str_val[1] = '\0';
            r.str_len = 1;
            return r;
        }
        case EXPR_SLICE: {
            EvalResult obj = eval_expr(expr->as.slice.object, st);
            EvalResult start = eval_expr(expr->as.slice.start, st);
            EvalResult end_val = eval_expr(expr->as.slice.end, st);
            if (start.type != VAL_INT || end_val.type != VAL_INT)
                diag_emit(expr->loc, DIAG_ERROR, "slice indices must be int");
            if (obj.type == VAL_ARRAY) {
                if (!obj.arr_val)
                    diag_emit(expr->loc, DIAG_ERROR, "slicing on null array");
                long s = start.int_val;
                long e = end_val.int_val;
                if (s < 0) s += obj.arr_val->count;
                if (e < 0) e += obj.arr_val->count;
                if (s < 0) s = 0;
                if (e > obj.arr_val->count) e = obj.arr_val->count;
                if (s > e) s = e;
                int cnt = (int)(e - s);
                ArrayData *arr = malloc(sizeof(ArrayData));
                arr->count = cnt;
                arr->elem_type = obj.arr_val->elem_type;
                arr->elements = malloc(cnt * sizeof(EvalResult));
                memcpy(arr->elements, obj.arr_val->elements + s, cnt * sizeof(EvalResult));
                r.type = VAL_ARRAY;
                r.arr_val = arr;
                return r;
            }
            if (obj.type != VAL_STRING)
                diag_emit(expr->loc, DIAG_ERROR, "slicing requires a string or array, got '%s'",
                          value_type_name(obj.type));
            long s = start.int_val;
            long e = end_val.int_val;
            if (s < 0) s += obj.str_len;
            if (e < 0) e += obj.str_len;
            if (s < 0) s = 0;
            if (e > obj.str_len) e = obj.str_len;
            if (s > e) s = e;
            int slen = (int)(e - s);
            r.type = VAL_STRING;
            r.str_val = malloc(slen + 1);
            memcpy(r.str_val, obj.str_val + s, slen);
            r.str_val[slen] = '\0';
            r.str_len = slen;
            return r;
        }
        case EXPR_ARRAY_LIT: {
            int count = expr->as.array_lit.count;
            ArrayData *arr = malloc(sizeof(ArrayData));
            arr->count = count;
            arr->elements = malloc((count > 0 ? count : 1) * sizeof(EvalResult));
            arr->elem_type = VAL_VOID; /* infer from first element */
            for (int ai = 0; ai < count; ai++) {
                arr->elements[ai] = eval_expr(expr->as.array_lit.elements[ai], st);
                if (ai == 0) {
                    arr->elem_type = arr->elements[ai].type;
                } else if (arr->elements[ai].type != arr->elem_type) {
                    diag_emit(expr->loc, DIAG_ERROR,
                              "array element type mismatch: expected '%s', got '%s'",
                              value_type_name(arr->elem_type),
                              value_type_name(arr->elements[ai].type));
                }
            }
            r.type = VAL_ARRAY;
            r.arr_val = arr;
            return r;
        }
        case EXPR_CHANNEL_LIT: {
            ChannelData *ch = malloc(sizeof(ChannelData));
            ch->cap = 16;
            ch->count = 0;
            ch->read_pos = 0;
            ch->elem_type = expr->as.channel_lit.elem_type;
            ch->buffer = malloc(ch->cap * sizeof(EvalResult));
            r.type = VAL_CHANNEL;
            r.chan_val = ch;
            return r;
        }
        case EXPR_FN_CALL: {
            int argc = expr->as.fn_call.arg_count;

            if (expr->as.fn_call.obj_name) {
                /* Method call in expression context: obj.method(args) */
                ASTNode tmp;
                memset(&tmp, 0, sizeof(tmp));
                tmp.loc = expr->loc;
                tmp.is_fn_call = 1;
                tmp.obj_name = expr->as.fn_call.obj_name;
                tmp.fn_name = expr->as.fn_call.fn_name;
                tmp.call_arg_count = argc;
                tmp.call_arg_exprs = expr->as.fn_call.args;
                tmp.call_arg_names = expr->as.fn_call.arg_names;
                tmp.call_arg_is_var_ref = calloc(argc, sizeof(int));
                EvalResult result = evaluate_method_call(&tmp, st, g_ft, g_ct, g_prints, 1);
                free(tmp.call_arg_is_var_ref);
                return result;
            }

            /* Free function call — evaluate args to EvalResult */
            EvalResult *arg_results = malloc((argc > 0 ? argc : 1) * sizeof(EvalResult));
            for (int i = 0; i < argc; i++)
                arg_results[i] = eval_expr(expr->as.fn_call.args[i], st);

            EvalResult result = evaluate_fn_call(g_ft, g_ct, st,
                                                 expr->as.fn_call.fn_name, expr->loc,
                                                 argc, arg_results,
                                                 expr->as.fn_call.arg_names, g_prints);
            free(arg_results);

            if (result.type == VAL_VOID)
                diag_emit(expr->loc, DIAG_ERROR, "cannot use void function result in expression");
            return result;
        }
    }
    return r;
}

/* Convert an EvalResult to a string for output */
static char *eval_to_string(EvalResult *r, int *out_len) {
    char buf[64];
    switch (r->type) {
        case VAL_STRING:
            *out_len = r->str_len;
            {
                char *s = malloc(r->str_len + 1);
                memcpy(s, r->str_val, r->str_len);
                s[r->str_len] = '\0';
                return s;
            }
        case VAL_INT:
            *out_len = snprintf(buf, sizeof(buf), "%ld", r->int_val);
            {
                char *s = malloc(*out_len + 1);
                memcpy(s, buf, *out_len + 1);
                return s;
            }
        case VAL_FLOAT:
            *out_len = snprintf(buf, sizeof(buf), "%g", r->float_val);
            {
                char *s = malloc(*out_len + 1);
                memcpy(s, buf, *out_len + 1);
                return s;
            }
        case VAL_BOOL:
            if (r->bool_val) {
                *out_len = 4;
                char *s = malloc(5);
                memcpy(s, "true", 5);
                return s;
            } else {
                *out_len = 5;
                char *s = malloc(6);
                memcpy(s, "false", 6);
                return s;
            }
        case VAL_OBJECT:
            if (r->obj_val) {
                /* Format: ClassName{field: val, ...} */
                int cap = 256;
                char *s = malloc(cap);
                int pos = snprintf(s, cap, "%s{", r->obj_val->class_name);
                for (int i = 0; i < r->obj_val->field_count; i++) {
                    if (i > 0) { pos += snprintf(s + pos, cap - pos, ", "); }
                    int flen;
                    char *fstr = eval_to_string(&r->obj_val->field_values[i], &flen);
                    int needed = pos + (int)strlen(r->obj_val->field_names[i]) + 2 + flen + 4;
                    if (needed > cap) {
                        cap = needed * 2;
                        s = realloc(s, cap);
                    }
                    pos += snprintf(s + pos, cap - pos, "%s: %s",
                                    r->obj_val->field_names[i], fstr);
                    free(fstr);
                }
                pos += snprintf(s + pos, cap - pos, "}");
                *out_len = pos;
                return s;
            }
            /* fallthrough */
        case VAL_ARRAY:
            if (r->arr_val) {
                int cap = 256;
                char *s = malloc(cap);
                int pos = 0;
                s[pos++] = '[';
                for (int i = 0; i < r->arr_val->count; i++) {
                    if (i > 0) {
                        if (pos + 2 >= cap) { cap *= 2; s = realloc(s, cap); }
                        s[pos++] = ',';
                        s[pos++] = ' ';
                    }
                    int flen;
                    char *fstr = eval_to_string(&r->arr_val->elements[i], &flen);
                    while (pos + flen + 2 >= cap) { cap *= 2; s = realloc(s, cap); }
                    memcpy(s + pos, fstr, flen);
                    pos += flen;
                    free(fstr);
                }
                if (pos + 1 >= cap) { cap = (pos + 2) * 2; s = realloc(s, cap); }
                s[pos++] = ']';
                s[pos] = '\0';
                *out_len = pos;
                return s;
            }
            {
                *out_len = 2;
                char *s = malloc(3);
                memcpy(s, "[]", 3);
                return s;
            }
        case VAL_CHANNEL:
            if (r->chan_val) {
                char buf[128];
                int items = r->chan_val->count - r->chan_val->read_pos;
                int n = snprintf(buf, sizeof(buf), "Channel<%s>(%d items)",
                                 value_type_name(r->chan_val->elem_type), items);
                *out_len = n;
                char *s = malloc(n + 1);
                memcpy(s, buf, n + 1);
                return s;
            }
            {
                const char *str = "Channel(empty)";
                *out_len = (int)strlen(str);
                char *s = malloc(*out_len + 1);
                memcpy(s, str, *out_len + 1);
                return s;
            }
        default:
            *out_len = 0;
            return malloc(1);
    }
}

/* ================================================================
 * Return context for propagating return statements through scopes
 * ================================================================ */

typedef struct {
    int has_return;
    EvalResult return_result;
    int has_break;
    int has_continue;
} ReturnCtx;

/* Forward declarations */
static void eval_stmts(ASTNode *stmts, SymTable *st, FnTable *ft, ClassTable *ct, PrintList *prints, ReturnCtx *ret);
static EvalResult evaluate_fn_call(FnTable *ft, ClassTable *ct, SymTable *outer_st,
                                   const char *fn_name, SourceLoc call_loc,
                                   int arg_count,
                                   EvalResult *arg_results,
                                   char **arg_names,
                                   PrintList *prints);

/* ================================================================
 * Helper: resolve fn call args to EvalResult array
 * ================================================================ */

static EvalResult *resolve_call_args_eval(ASTNode *n, SymTable *st) {
    EvalResult *results = malloc((n->call_arg_count > 0 ? n->call_arg_count : 1) * sizeof(EvalResult));
    for (int i = 0; i < n->call_arg_count; i++) {
        if (n->call_arg_exprs && n->call_arg_exprs[i]) {
            results[i] = eval_expr(n->call_arg_exprs[i], st);
        } else if (n->call_arg_is_var_ref && n->call_arg_is_var_ref[i]) {
            Symbol *sym = sym_find(st, n->call_args[i]);
            if (!sym)
                diag_emit(n->loc, DIAG_ERROR, "undefined variable '%s'", n->call_args[i]);
            results[i] = sym->val;
        } else if (n->call_args && n->call_args[i]) {
            memset(&results[i], 0, sizeof(EvalResult));
            results[i].type = n->call_arg_types[i];
            switch (n->call_arg_types[i]) {
                case VAL_INT:    results[i].int_val = atol(n->call_args[i]); break;
                case VAL_FLOAT:  results[i].float_val = atof(n->call_args[i]); break;
                case VAL_STRING: results[i].str_val = n->call_args[i];
                                 results[i].str_len = (int)strlen(n->call_args[i]); break;
                case VAL_BOOL:   results[i].bool_val = (strcmp(n->call_args[i], "true") == 0); break;
                default: break;
            }
        } else {
            memset(&results[i], 0, sizeof(EvalResult));
        }
    }
    return results;
}

/* ================================================================
 * Helper: evaluate a fn-call RHS and return EvalResult
 * ================================================================ */

static EvalResult eval_fn_call_result(ASTNode *n, SymTable *st, FnTable *ft,
                                      ClassTable *ct, PrintList *prints, int require_value) {
    EvalResult *arg_results = resolve_call_args_eval(n, st);
    EvalResult result = evaluate_fn_call(ft, ct, st, n->fn_name, n->loc,
                                         n->call_arg_count, arg_results,
                                         n->call_arg_names, prints);
    free(arg_results);
    if (require_value && result.type == VAL_VOID)
        diag_emit(n->loc, DIAG_ERROR, "cannot use void function result");
    return result;
}

/* ================================================================
 * eval_new_expr — construct a new object instance
 * ================================================================ */

static EvalResult eval_new_expr(ASTNode *n, SymTable *st, ClassTable *ct) {
    ClassDef *cls = class_table_find(ct, n->fn_name);
    if (!cls)
        diag_emit(n->loc, DIAG_ERROR, "undefined class '%s'", n->fn_name);

    /* Resolve arguments */
    int arg_count = n->call_arg_count;
    EvalResult *arg_vals = malloc(arg_count * sizeof(EvalResult));
    for (int i = 0; i < arg_count; i++) {
        if (n->call_arg_exprs && n->call_arg_exprs[i]) {
            arg_vals[i] = eval_expr(n->call_arg_exprs[i], st);
        } else if (n->call_arg_is_var_ref[i]) {
            Symbol *sym = sym_find(st, n->call_args[i]);
            if (!sym)
                diag_emit(n->loc, DIAG_ERROR, "undefined variable '%s'", n->call_args[i]);
            arg_vals[i] = sym->val;
        } else {
            memset(&arg_vals[i], 0, sizeof(EvalResult));
            arg_vals[i].type = n->call_arg_types[i];
            switch (n->call_arg_types[i]) {
                case VAL_INT:    arg_vals[i].int_val = atol(n->call_args[i]); break;
                case VAL_FLOAT:  arg_vals[i].float_val = atof(n->call_args[i]); break;
                case VAL_STRING: arg_vals[i].str_val = n->call_args[i];
                                 arg_vals[i].str_len = (int)strlen(n->call_args[i]); break;
                case VAL_BOOL:   arg_vals[i].bool_val = (strcmp(n->call_args[i], "true") == 0); break;
                default: break;
            }
        }
    }

    /* Match args to class fields (same logic as fn call: positional then named) */
    ObjData *obj = malloc(sizeof(ObjData));
    obj->class_name = cls->name;
    obj->field_count = cls->field_count;
    obj->field_names = cls->field_names;
    obj->field_values = malloc(cls->field_count * sizeof(EvalResult));
    int *filled = calloc(cls->field_count, sizeof(int));

    int has_named = 0;
    if (n->call_arg_names) {
        for (int i = 0; i < arg_count; i++)
            if (n->call_arg_names[i]) { has_named = 1; break; }
    }

    if (has_named) {
        int pos_idx = 0;
        for (int i = 0; i < arg_count; i++) {
            if (n->call_arg_names && n->call_arg_names[i]) continue;
            if (pos_idx >= cls->field_count)
                diag_emit(n->loc, DIAG_ERROR, "too many positional arguments for class '%s'", cls->name);
            obj->field_values[pos_idx] = arg_vals[i];
            filled[pos_idx] = 1;
            pos_idx++;
        }
        for (int i = 0; i < arg_count; i++) {
            if (!n->call_arg_names || !n->call_arg_names[i]) continue;
            int found = 0;
            for (int f = 0; f < cls->field_count; f++) {
                if (strcmp(n->call_arg_names[i], cls->field_names[f]) == 0) {
                    if (filled[f])
                        diag_emit(n->loc, DIAG_ERROR, "duplicate argument for field '%s' in class '%s'",
                                  n->call_arg_names[i], cls->name);
                    obj->field_values[f] = arg_vals[i];
                    filled[f] = 1;
                    found = 1;
                    break;
                }
            }
            if (!found)
                diag_emit(n->loc, DIAG_ERROR, "unknown field '%s' in class '%s'",
                          n->call_arg_names[i], cls->name);
        }
    } else {
        if (arg_count != cls->field_count)
            diag_emit(n->loc, DIAG_ERROR, "class '%s' has %d field(s), got %d argument(s)",
                      cls->name, cls->field_count, arg_count);
        for (int i = 0; i < arg_count; i++) {
            obj->field_values[i] = arg_vals[i];
            filled[i] = 1;
        }
    }

    /* Check all fields filled and type-check */
    for (int i = 0; i < cls->field_count; i++) {
        if (!filled[i])
            diag_emit(n->loc, DIAG_ERROR, "missing value for field '%s' in class '%s'",
                      cls->field_names[i], cls->name);
        if (obj->field_values[i].type != cls->field_types[i])
            diag_emit(n->loc, DIAG_ERROR, "field '%s' expects '%s', got '%s'",
                      cls->field_names[i], value_type_name(cls->field_types[i]),
                      value_type_name(obj->field_values[i].type));
    }

    free(filled);
    free(arg_vals);

    EvalResult val;
    memset(&val, 0, sizeof(val));
    val.type = VAL_OBJECT;
    val.obj_val = obj;
    return val;
}

/* ================================================================
 * evaluate_method_call — call a method on an object
 * ================================================================ */

static EvalResult evaluate_method_call(ASTNode *n, SymTable *st, FnTable *ft,
                                       ClassTable *ct, PrintList *prints,
                                       int require_value) {
    Symbol *obj_sym = sym_find(st, n->obj_name);
    if (!obj_sym)
        diag_emit(n->loc, DIAG_ERROR, "undefined variable '%s'", n->obj_name);
    if (obj_sym->val.type != VAL_OBJECT || !obj_sym->val.obj_val)
        diag_emit(n->loc, DIAG_ERROR, "'%s' is not an object", n->obj_name);

    ObjData *obj = obj_sym->val.obj_val;
    const char *method_name = n->fn_name;

    /* Walk inheritance chain to find method */
    ASTNode *method_decl = NULL;
    ClassDef *cls = class_table_find(ct, obj->class_name);
    while (cls) {
        for (ASTNode *m = cls->methods; m; m = m->next) {
            if (strcmp(m->fn_name, method_name) == 0) {
                method_decl = m;
                break;
            }
        }
        if (method_decl) break;
        cls = cls->parent_name ? class_table_find(ct, cls->parent_name) : NULL;
    }
    if (!method_decl)
        diag_emit(n->loc, DIAG_ERROR, "no method '%s' on class '%s'",
                  method_name, obj->class_name);

    /* Build local scope: object fields + method params */
    SymTable local_st;
    sym_table_init(&local_st);
    local_st.parent = st;

    /* Add object fields as local variables */
    for (int i = 0; i < obj->field_count; i++) {
        sym_add(&local_st, obj->field_names[i], obj->field_values[i], 0, n->loc);
    }

    /* Resolve and add method parameters */
    int arg_count = n->call_arg_count;
    for (int i = 0; i < arg_count; i++) {
        EvalResult pval;
        memset(&pval, 0, sizeof(pval));
        if (n->call_arg_exprs && n->call_arg_exprs[i]) {
            pval = eval_expr(n->call_arg_exprs[i], st);
        } else if (n->call_arg_is_var_ref[i]) {
            Symbol *asym = sym_find(st, n->call_args[i]);
            if (!asym)
                diag_emit(n->loc, DIAG_ERROR, "undefined variable '%s'", n->call_args[i]);
            pval = asym->val;
        } else {
            pval.type = n->call_arg_types[i];
            switch (n->call_arg_types[i]) {
                case VAL_INT:    pval.int_val = atol(n->call_args[i]); break;
                case VAL_FLOAT:  pval.float_val = atof(n->call_args[i]); break;
                case VAL_STRING: pval.str_val = n->call_args[i]; pval.str_len = (int)strlen(n->call_args[i]); break;
                case VAL_BOOL:   pval.bool_val = (strcmp(n->call_args[i], "true") == 0); break;
                default: break;
            }
        }
        if (i < method_decl->param_count) {
            if (pval.type != method_decl->params[i].type)
                diag_emit(n->loc, DIAG_ERROR, "method '%s' parameter '%s' expects '%s', got '%s'",
                          method_name, method_decl->params[i].name,
                          value_type_name(method_decl->params[i].type),
                          value_type_name(pval.type));
            sym_add(&local_st, method_decl->params[i].name, pval, 1, n->loc);
        }
    }

    if (arg_count < method_decl->param_count) {
        /* Fill defaults */
        for (int i = arg_count; i < method_decl->param_count; i++) {
            if (!method_decl->params[i].has_default)
                diag_emit(n->loc, DIAG_ERROR, "missing argument for parameter '%s' in method '%s'",
                          method_decl->params[i].name, method_name);
            EvalResult pval;
            memset(&pval, 0, sizeof(pval));
            pval.type = method_decl->params[i].type;
            switch (pval.type) {
                case VAL_INT:    pval.int_val = atol(method_decl->params[i].default_value); break;
                case VAL_FLOAT:  pval.float_val = atof(method_decl->params[i].default_value); break;
                case VAL_STRING: pval.str_val = method_decl->params[i].default_value;
                                 pval.str_len = method_decl->params[i].default_value_len; break;
                case VAL_BOOL:   pval.bool_val = (strcmp(method_decl->params[i].default_value, "true") == 0); break;
                default: break;
            }
            sym_add(&local_st, method_decl->params[i].name, pval, 1, n->loc);
        }
    }

    ReturnCtx ret_ctx;
    memset(&ret_ctx, 0, sizeof(ret_ctx));

    eval_stmts(method_decl->body, &local_st, ft, ct, prints, &ret_ctx);

    /* Propagate field mutations back to the object */
    for (int i = 0; i < obj->field_count; i++) {
        int idx = sym_lookup(&local_st, obj->field_names[i]);
        if (idx >= 0)
            obj->field_values[i] = local_st.syms[idx].val;
    }

    sym_table_free(&local_st);

    if (method_decl->has_return_type && !ret_ctx.has_return)
        diag_emit(n->loc, DIAG_ERROR, "method '%s' must return a value", method_name);
    if (ret_ctx.has_return && method_decl->has_return_type &&
        ret_ctx.return_result.type != method_decl->return_type)
        diag_emit(n->loc, DIAG_ERROR, "method '%s' returns '%s', expected '%s'",
                  method_name, value_type_name(ret_ctx.return_result.type),
                  value_type_name(method_decl->return_type));

    EvalResult result;
    memset(&result, 0, sizeof(result));
    if (require_value && !ret_ctx.has_return)
        diag_emit(n->loc, DIAG_ERROR, "cannot use void method result");
    if (ret_ctx.has_return) {
        result = ret_ctx.return_result;
    }
    return result;
}

/* ================================================================
 * eval_stmts — unified statement evaluator with block scoping
 * ================================================================ */

/* ================================================================
 * Flush pending compile-time prints to IR
 * ================================================================ */

static void flush_prints_to_ir(PrintList *prints) {
    if (!g_ir_mode && g_ir) {
        for (int pi = 0; pi < prints->count; pi++) {
            ir_emit_print_str(g_ir, prints->strings[pi], prints->lengths[pi]);
        }
        g_ir_mode = 1;
    }
}

/* ================================================================
 * ir_compile_stmts — compile an AST statement list to IR
 *
 * Used when control flow depends on runtime values (if/else, for loops).
 * Once we enter this function, everything is compiled to IR instructions.
 * ================================================================ */

static void ir_compile_stmts(ASTNode *stmts, SymTable *st, IRProgram *prog,
                              int break_label, int continue_label) {
    for (ASTNode *n = stmts; n; n = n->next) {
        if (n->type == NODE_FN_DECL || n->type == NODE_CLASS_DECL ||
            n->type == NODE_ENUM_DECL || n->type == NODE_IMPORT)
            continue;

        if (n->type == NODE_BREAK) {
            if (break_label >= 0)
                ir_emit_jmp(prog, break_label);
            return;
        }

        if (n->type == NODE_CONTINUE) {
            if (continue_label >= 0)
                ir_emit_jmp(prog, continue_label);
            return;
        }

        if (n->type == NODE_VAR_DECL) {
            /* Evaluate initializer at compile time, add to symbol table,
             * then allocate an IR slot for mutable int/bool */
            EvalResult val;
            if (n->is_new_expr) {
                val = eval_new_expr(n, st, g_ct);
            } else if (n->is_fn_call && n->obj_name) {
                val = evaluate_method_call(n, st, g_ft, g_ct, g_prints, 1);
            } else if (n->is_fn_call) {
                val = eval_fn_call_result(n, st, g_ft, g_ct, g_prints, 1);
            } else {
                val = eval_expr(n->expr, st);
            }
            sym_add(st, n->var_name, val, n->is_const, n->loc);

            if (!n->is_const && (val.type == VAL_INT || val.type == VAL_BOOL)) {
                int slot = ir_alloc_slot(prog);
                st->syms[st->count - 1].has_slot = 1;
                st->syms[st->count - 1].slot = slot;
                int init_vreg;
                if (n->expr && expr_is_runtime(n->expr, st)) {
                    init_vreg = ir_compile_expr(n->expr, st, prog);
                } else {
                    int64_t iv = (val.type == VAL_INT) ? val.int_val : (int64_t)val.bool_val;
                    init_vreg = ir_emit_const_int(prog, iv);
                }
                ir_emit_store(prog, slot, init_vreg);
            }
        } else if (n->type == NODE_ASSIGN) {
            if (n->field_name) {
                /* Field assignment: obj.field = value; — compile-time only in IR mode */
                Symbol *sym = sym_find(st, n->var_name);
                if (!sym) diag_emit(n->loc, DIAG_ERROR, "undefined variable '%s'", n->var_name);
                if (sym->is_const) diag_emit(n->loc, DIAG_ERROR, "cannot mutate fields of const variable '%s'", n->var_name);
                if (sym->val.type != VAL_OBJECT || !sym->val.obj_val)
                    diag_emit(n->loc, DIAG_ERROR, "'%s' is not an object", n->var_name);
                ObjData *obj = sym->val.obj_val;
                int found = 0;
                for (int i = 0; i < obj->field_count; i++) {
                    if (strcmp(obj->field_names[i], n->field_name) == 0) {
                        EvalResult fval = eval_expr(n->expr, st);
                        if (obj->field_values[i].type != fval.type)
                            diag_emit(n->loc, DIAG_ERROR, "type mismatch for field '%s'", n->field_name);
                        obj->field_values[i] = fval;
                        found = 1;
                        break;
                    }
                }
                if (!found)
                    diag_emit(n->loc, DIAG_ERROR, "no field '%s' on object of class '%s'", n->field_name, obj->class_name);
                sym->mutated = 1;
            } else {
                Symbol *sym = sym_find(st, n->var_name);
                if (!sym) diag_emit(n->loc, DIAG_ERROR, "undefined variable '%s'", n->var_name);
                if (sym->is_const) diag_emit(n->loc, DIAG_ERROR, "cannot reassign const variable '%s'", n->var_name);

                if (sym->has_slot) {
                    /* Emit IR store */
                    EvalResult val = eval_expr(n->expr, st);
                    if (sym->val.type != val.type)
                        diag_emit(n->loc, DIAG_ERROR, "type mismatch: variable '%s' has type '%s', cannot assign '%s'",
                                  n->var_name, value_type_name(sym->val.type), value_type_name(val.type));
                    sym->val = val;
                    sym->mutated = 1;
                    int src_vreg;
                    if (n->expr && expr_is_runtime(n->expr, st)) {
                        src_vreg = ir_compile_expr(n->expr, st, prog);
                    } else {
                        int64_t cv = (val.type == VAL_INT) ? val.int_val : (int64_t)val.bool_val;
                        src_vreg = ir_emit_const_int(prog, cv);
                    }
                    ir_emit_store(prog, sym->slot, src_vreg);
                } else {
                    EvalResult val = eval_expr(n->expr, st);
                    if (sym->val.type != val.type)
                        diag_emit(n->loc, DIAG_ERROR, "type mismatch: variable '%s' has type '%s', cannot assign '%s'",
                                  n->var_name, value_type_name(sym->val.type), value_type_name(val.type));
                    sym->val = val;
                    sym->mutated = 1;
                }
            }
        } else if (n->type == NODE_PRINT) {
            if (n->expr && expr_is_runtime(n->expr, st)) {
                int vreg = ir_compile_expr(n->expr, st, prog);
                ValueType rt = expr_runtime_type(n->expr, st);
                if (rt == VAL_BOOL) {
                    ir_emit_print_bool(prog, vreg);
                } else {
                    ir_emit_print_int(prog, vreg);
                }
            } else {
                EvalResult val;
                if (n->is_fn_call && n->obj_name) {
                    val = evaluate_method_call(n, st, g_ft, g_ct, g_prints, 1);
                } else if (n->is_fn_call) {
                    val = eval_fn_call_result(n, st, g_ft, g_ct, g_prints, 1);
                } else {
                    val = eval_expr(n->expr, st);
                }
                int slen;
                char *s = eval_to_string(&val, &slen);
                ir_emit_print_str(prog, s, slen);
            }
            if (n->print_newline) {
                ir_emit_print_str(prog, "\n", 1);
            }
        } else if (n->type == NODE_IF_STMT) {
            /* Compile if/else chain to IR */
            ASTNode *branch = n;
            int end_label = ir_alloc_label(prog);

            while (branch && branch->type == NODE_IF_STMT && branch->if_cond) {
                int cond_vreg = ir_compile_expr(branch->if_cond, st, prog);
                int else_label = ir_alloc_label(prog);
                ir_emit_jz(prog, cond_vreg, else_label);

                /* Compile if-body */
                SymTable if_st;
                sym_table_init(&if_st);
                if_st.parent = st;
                ir_compile_stmts(branch->if_body, &if_st, prog, break_label, continue_label);
                sym_table_free(&if_st);
                ir_emit_jmp(prog, end_label);

                ir_emit_label(prog, else_label);
                branch = branch->else_body;
            }

            /* else branch (if any) */
            if (branch && branch->type != NODE_IF_STMT) {
                SymTable else_st;
                sym_table_init(&else_st);
                else_st.parent = st;
                ir_compile_stmts(branch, &else_st, prog, break_label, continue_label);
                sym_table_free(&else_st);
            }

            ir_emit_label(prog, end_label);
        } else if (n->type == NODE_FOR_LOOP) {
            /* Compile for loop to IR */
            SymTable loop_st;
            sym_table_init(&loop_st);
            loop_st.parent = st;

            /* Compile for_init */
            ir_compile_stmts(n->for_init, &loop_st, prog, -1, -1);

            int loop_start = ir_alloc_label(prog);
            int loop_end = ir_alloc_label(prog);
            int loop_continue = ir_alloc_label(prog);

            ir_emit_label(prog, loop_start);

            /* Compile condition */
            int cond_vreg = ir_compile_expr(n->for_cond, &loop_st, prog);
            ir_emit_jz(prog, cond_vreg, loop_end);

            /* Compile body */
            SymTable body_st;
            sym_table_init(&body_st);
            body_st.parent = &loop_st;
            ir_compile_stmts(n->body, &body_st, prog, loop_end, loop_continue);
            sym_table_free(&body_st);

            /* Continue label */
            ir_emit_label(prog, loop_continue);

            /* Compile update */
            ir_compile_stmts(n->for_update, &loop_st, prog, -1, -1);

            ir_emit_jmp(prog, loop_start);

            ir_emit_label(prog, loop_end);

            sym_table_free(&loop_st);
        } else if (n->type == NODE_BLOCK) {
            SymTable child;
            sym_table_init(&child);
            child.parent = st;
            ir_compile_stmts(n->body, &child, prog, break_label, continue_label);
            sym_table_free(&child);
        } else if (n->type == NODE_MATCH_STMT) {
            /* Compile match statement to IR */
            int scrutinee_vreg = ir_compile_expr(n->match_expr, st, prog);
            int end_label = ir_alloc_label(prog);

            for (int a = 0; a < n->match_arm_count; a++) {
                MatchArm *arm = &n->match_arms[a];
                int next_arm_label = ir_alloc_label(prog);

                if (!arm->is_wildcard) {
                    /* Compare scrutinee against pattern */
                    int pattern_vreg = ir_compile_expr(arm->pattern, st, prog);
                    int cmp_vreg = ir_emit_binop(prog, IR_CMP_NE, scrutinee_vreg, pattern_vreg);
                    ir_emit_jnz(prog, cmp_vreg, next_arm_label);
                }

                /* Compile arm body */
                SymTable match_st;
                sym_table_init(&match_st);
                match_st.parent = st;
                ir_compile_stmts(arm->body, &match_st, prog, break_label, continue_label);
                sym_table_free(&match_st);
                ir_emit_jmp(prog, end_label);

                ir_emit_label(prog, next_arm_label);
            }

            ir_emit_label(prog, end_label);
        } else if (n->type == NODE_SPAWN) {
            /* spawn fn_call; — execute at compile time, discard result (same as eval_stmts) */
            Expr *call_expr = n->spawn_expr;
            int argc = call_expr->as.fn_call.arg_count;
            if (call_expr->as.fn_call.obj_name) {
                ASTNode tmp;
                memset(&tmp, 0, sizeof(tmp));
                tmp.loc = n->loc;
                tmp.is_fn_call = 1;
                tmp.obj_name = call_expr->as.fn_call.obj_name;
                tmp.fn_name = call_expr->as.fn_call.fn_name;
                tmp.call_arg_count = argc;
                tmp.call_arg_exprs = call_expr->as.fn_call.args;
                tmp.call_arg_names = call_expr->as.fn_call.arg_names;
                tmp.call_arg_is_var_ref = calloc(argc, sizeof(int));
                (void)evaluate_method_call(&tmp, st, g_ft, g_ct, g_prints, 0);
                free(tmp.call_arg_is_var_ref);
            } else {
                EvalResult *arg_results = malloc((argc > 0 ? argc : 1) * sizeof(EvalResult));
                for (int i = 0; i < argc; i++)
                    arg_results[i] = eval_expr(call_expr->as.fn_call.args[i], st);
                (void)evaluate_fn_call(g_ft, g_ct, st, call_expr->as.fn_call.fn_name,
                                       n->loc, argc, arg_results,
                                       call_expr->as.fn_call.arg_names, g_prints);
                free(arg_results);
            }
        } else if (n->type == NODE_FN_CALL) {
            if (n->obj_name) {
                evaluate_method_call(n, st, g_ft, g_ct, g_prints, 0);
            } else {
                EvalResult *arg_results = resolve_call_args_eval(n, st);
                (void)evaluate_fn_call(g_ft, g_ct, st, n->fn_name, n->loc,
                                       n->call_arg_count, arg_results,
                                       n->call_arg_names, g_prints);
                free(arg_results);
            }
        }
    }
}

static void eval_stmts(ASTNode *stmts, SymTable *st, FnTable *ft, ClassTable *ct, PrintList *prints, ReturnCtx *ret) {
    for (ASTNode *n = stmts; n; n = n->next) {
        if (ret && (ret->has_return || ret->has_break || ret->has_continue)) return;

        if (n->type == NODE_FN_DECL) continue;
        if (n->type == NODE_CLASS_DECL) continue; /* collected in first pass */
        if (n->type == NODE_ENUM_DECL) continue;  /* collected in first pass */
        if (n->type == NODE_IMPORT) continue;     /* processed in pass 0 */

        if (n->type == NODE_BREAK) {
            if (ret) ret->has_break = 1;
            return;
        }

        if (n->type == NODE_CONTINUE) {
            if (ret) ret->has_continue = 1;
            return;
        }

        if (n->type == NODE_RETURN) {
            if (!ret) {
                diag_emit(n->loc, DIAG_ERROR, "return statement outside of function");
                return;
            }
            if (n->is_fn_call && n->obj_name) {
                /* return obj.method(args); */
                ret->return_result = evaluate_method_call(n, st, ft, ct, prints, 1);
            } else if (n->is_fn_call) {
                EvalResult *arg_results = resolve_call_args_eval(n, st);
                ret->return_result = evaluate_fn_call(ft, ct, st, n->fn_name, n->loc,
                                                      n->call_arg_count, arg_results,
                                                      n->call_arg_names, prints);
                free(arg_results);
            } else if (n->expr) {
                ret->return_result = eval_expr(n->expr, st);
            }
            ret->has_return = 1;
            return;
        }

        if (n->type == NODE_VAR_DECL) {
            EvalResult val;
            if (n->is_new_expr) {
                val = eval_new_expr(n, st, ct);
            } else if (n->is_fn_call && n->obj_name) {
                val = evaluate_method_call(n, st, ft, ct, prints, 1);
            } else if (n->is_fn_call) {
                val = eval_fn_call_result(n, st, ft, ct, prints, 1);
            } else {
                val = eval_expr(n->expr, st);
            }
            sym_add(st, n->var_name, val, n->is_const, n->loc);

            /* IR: allocate a runtime slot for mutable int/bool variables */
            if (g_ir && !n->is_const && (val.type == VAL_INT || val.type == VAL_BOOL)) {
                int slot = ir_alloc_slot(g_ir);
                st->syms[st->count - 1].has_slot = 1;
                st->syms[st->count - 1].slot = slot;
                /* Emit initial store */
                int init_vreg;
                if (n->expr && expr_is_runtime(n->expr, st)) {
                    init_vreg = ir_compile_expr(n->expr, st, g_ir);
                } else {
                    int64_t init_val = (val.type == VAL_INT) ? val.int_val : (int64_t)val.bool_val;
                    init_vreg = ir_emit_const_int(g_ir, init_val);
                }
                ir_emit_store(g_ir, slot, init_vreg);
            }
        } else if (n->type == NODE_ASSIGN) {
            if (n->field_name) {
                /* Field assignment: obj.field = value; */
                Symbol *sym = sym_find(st, n->var_name);
                if (!sym)
                    diag_emit(n->loc, DIAG_ERROR, "undefined variable '%s'", n->var_name);
                if (sym->is_const)
                    diag_emit(n->loc, DIAG_ERROR, "cannot mutate fields of const variable '%s'", n->var_name);
                if (sym->val.type != VAL_OBJECT || !sym->val.obj_val)
                    diag_emit(n->loc, DIAG_ERROR, "'%s' is not an object", n->var_name);
                ObjData *obj = sym->val.obj_val;
                int found = 0;
                for (int i = 0; i < obj->field_count; i++) {
                    if (strcmp(obj->field_names[i], n->field_name) == 0) {
                        EvalResult val;
                        if (n->is_fn_call && n->obj_name) {
                            val = evaluate_method_call(n, st, ft, ct, prints, 1);
                        } else if (n->is_fn_call) {
                            val = eval_fn_call_result(n, st, ft, ct, prints, 1);
                        } else {
                            val = eval_expr(n->expr, st);
                        }
                        if (obj->field_values[i].type != val.type)
                            diag_emit(n->loc, DIAG_ERROR,
                                      "type mismatch: field '%s' has type '%s', cannot assign '%s'",
                                      n->field_name, value_type_name(obj->field_values[i].type),
                                      value_type_name(val.type));
                        obj->field_values[i] = val;
                        found = 1;
                        break;
                    }
                }
                if (!found)
                    diag_emit(n->loc, DIAG_ERROR, "no field '%s' on object of class '%s'",
                              n->field_name, obj->class_name);
                sym->mutated = 1;
            } else {
                /* Regular assignment */
                Symbol *sym = sym_find(st, n->var_name);
                if (!sym)
                    diag_emit(n->loc, DIAG_ERROR, "undefined variable '%s'", n->var_name);
                if (sym->is_const)
                    diag_emit(n->loc, DIAG_ERROR, "cannot reassign const variable '%s'", n->var_name);

                /* IR path: if the target has a slot, compile the RHS to IR */
                if (g_ir && sym->has_slot) {
                    EvalResult val;
                    if (n->is_fn_call && n->obj_name) {
                        val = evaluate_method_call(n, st, ft, ct, prints, 1);
                    } else if (n->is_fn_call) {
                        val = eval_fn_call_result(n, st, ft, ct, prints, 1);
                    } else {
                        val = eval_expr(n->expr, st);
                    }
                    if (sym->val.type != val.type)
                        diag_emit(n->loc, DIAG_ERROR, "type mismatch: variable '%s' has type '%s', cannot assign '%s'",
                                  n->var_name, value_type_name(sym->val.type), value_type_name(val.type));
                    sym->val = val;
                    sym->mutated = 1;
                    /* Emit IR store */
                    int src_vreg;
                    if (n->expr && expr_is_runtime(n->expr, st)) {
                        src_vreg = ir_compile_expr(n->expr, st, g_ir);
                    } else {
                        int64_t cv = (val.type == VAL_INT) ? val.int_val : (int64_t)val.bool_val;
                        src_vreg = ir_emit_const_int(g_ir, cv);
                    }
                    ir_emit_store(g_ir, sym->slot, src_vreg);
                } else {
                    EvalResult val;
                    if (n->is_fn_call && n->obj_name) {
                        val = evaluate_method_call(n, st, ft, ct, prints, 1);
                    } else if (n->is_fn_call) {
                        val = eval_fn_call_result(n, st, ft, ct, prints, 1);
                    } else {
                        val = eval_expr(n->expr, st);
                    }
                    if (sym->val.type != val.type)
                        diag_emit(n->loc, DIAG_ERROR, "type mismatch: variable '%s' has type '%s', cannot assign '%s'",
                                  n->var_name, value_type_name(sym->val.type), value_type_name(val.type));
                    sym->val = val;
                    sym->mutated = 1;
                }
            }
        } else if (n->type == NODE_PRINT) {
            /* Check if expression involves runtime variables */
            int is_rt = g_ir && n->expr && expr_is_runtime(n->expr, st);

            if (is_rt) {
                /* Flush any pending compile-time prints to IR first */
                if (!g_ir_mode) {
                    for (int pi = 0; pi < prints->count; pi++) {
                        ir_emit_print_str(g_ir, prints->strings[pi], prints->lengths[pi]);
                    }
                    g_ir_mode = 1;
                }
                /* Compile expr to IR and emit appropriate print */
                int vreg = ir_compile_expr(n->expr, st, g_ir);
                ValueType rt = expr_runtime_type(n->expr, st);
                if (rt == VAL_BOOL) {
                    ir_emit_print_bool(g_ir, vreg);
                } else {
                    ir_emit_print_int(g_ir, vreg);
                }
                if (n->print_newline) {
                    ir_emit_print_str(g_ir, "\n", 1);
                }
            } else {
                EvalResult val;
                if (n->is_fn_call && n->obj_name) {
                    val = evaluate_method_call(n, st, ft, ct, prints, 1);
                } else if (n->is_fn_call) {
                    val = eval_fn_call_result(n, st, ft, ct, prints, 1);
                } else {
                    val = eval_expr(n->expr, st);
                }
                int slen;
                char *s = eval_to_string(&val, &slen);

                if (g_ir_mode) {
                    /* Already in IR mode — emit as IR_PRINT_STR */
                    ir_emit_print_str(g_ir, s, slen);
                    if (n->print_newline) {
                        ir_emit_print_str(g_ir, "\n", 1);
                    }
                } else {
                    print_list_add(prints, s, slen);
                    if (n->print_newline) {
                        print_list_add(prints, "\n", 1);
                    }
                }
            }
        } else if (n->type == NODE_FN_CALL) {
            if (n->obj_name) {
                /* Standalone method call: obj.method(args); */
                evaluate_method_call(n, st, ft, ct, prints, 0);
            } else {
                EvalResult *arg_results = resolve_call_args_eval(n, st);
                (void)evaluate_fn_call(ft, ct, st, n->fn_name, n->loc,
                                       n->call_arg_count, arg_results,
                                       n->call_arg_names, prints);
                free(arg_results);
            }
        } else if (n->type == NODE_SPAWN) {
            /* spawn fn_call; — execute immediately at compile time, discard result */
            Expr *call_expr = n->spawn_expr;
            int argc = call_expr->as.fn_call.arg_count;
            if (call_expr->as.fn_call.obj_name) {
                /* Method call: obj.method(args) */
                ASTNode tmp;
                memset(&tmp, 0, sizeof(tmp));
                tmp.loc = n->loc;
                tmp.is_fn_call = 1;
                tmp.obj_name = call_expr->as.fn_call.obj_name;
                tmp.fn_name = call_expr->as.fn_call.fn_name;
                tmp.call_arg_count = argc;
                tmp.call_arg_exprs = call_expr->as.fn_call.args;
                tmp.call_arg_names = call_expr->as.fn_call.arg_names;
                tmp.call_arg_is_var_ref = calloc(argc, sizeof(int));
                (void)evaluate_method_call(&tmp, st, ft, ct, prints, 0);
                free(tmp.call_arg_is_var_ref);
            } else {
                EvalResult *arg_results = malloc((argc > 0 ? argc : 1) * sizeof(EvalResult));
                for (int i = 0; i < argc; i++)
                    arg_results[i] = eval_expr(call_expr->as.fn_call.args[i], st);
                (void)evaluate_fn_call(ft, ct, st, call_expr->as.fn_call.fn_name,
                                       n->loc, argc, arg_results,
                                       call_expr->as.fn_call.arg_names, prints);
                free(arg_results);
            }
        } else if (n->type == NODE_BLOCK) {
            SymTable child;
            sym_table_init(&child);
            child.parent = st;
            eval_stmts(n->body, &child, ft, ct, prints, ret);
            for (int j = 0; j < child.count; j++) {
                if (!child.syms[j].is_const && !child.syms[j].mutated)
                    diag_emit(child.syms[j].loc, DIAG_WARNING, "variable '%s' is never mutated, consider using 'const'", child.syms[j].name);
            }
            sym_table_free(&child);
        } else if (n->type == NODE_FOR_LOOP) {
            SymTable loop_st;
            sym_table_init(&loop_st);
            loop_st.parent = st;
            eval_stmts(n->for_init, &loop_st, ft, ct, prints, NULL);

            /* Check if the loop variable has an IR slot (runtime for loop) */
            int has_rt_loop_var = 0;
            if (g_ir) {
                for (int j = 0; j < loop_st.count; j++) {
                    if (loop_st.syms[j].has_slot) { has_rt_loop_var = 1; break; }
                }
                /* Also check if condition involves any runtime vars */
                if (!has_rt_loop_var && expr_is_runtime(n->for_cond, &loop_st))
                    has_rt_loop_var = 1;
            }

            if (has_rt_loop_var) {
                /* Runtime for loop — compile to IR */
                flush_prints_to_ir(prints);

                int loop_start = ir_alloc_label(g_ir);
                int loop_end = ir_alloc_label(g_ir);
                int loop_continue = ir_alloc_label(g_ir);

                ir_emit_label(g_ir, loop_start);

                int cond_vreg = ir_compile_expr(n->for_cond, &loop_st, g_ir);
                ir_emit_jz(g_ir, cond_vreg, loop_end);

                SymTable body_st;
                sym_table_init(&body_st);
                body_st.parent = &loop_st;
                ir_compile_stmts(n->body, &body_st, g_ir, loop_end, loop_continue);
                sym_table_free(&body_st);

                ir_emit_label(g_ir, loop_continue);

                ir_compile_stmts(n->for_update, &loop_st, g_ir, -1, -1);

                ir_emit_jmp(g_ir, loop_start);

                ir_emit_label(g_ir, loop_end);

                sym_table_free(&loop_st);
            } else {
                /* Compile-time for loop — original path */
                for (int iter = 0; ; iter++) {
                    if (iter >= 10000)
                        diag_emit(n->loc, DIAG_ERROR, "for loop exceeded 10000 iterations (possible infinite loop)");
                    EvalResult cond = eval_expr(n->for_cond, &loop_st);
                    if (cond.type != VAL_BOOL)
                        diag_emit(n->loc, DIAG_ERROR, "for loop condition must be a bool");
                    if (!cond.bool_val) break;
                    ReturnCtx loop_ret;
                    memset(&loop_ret, 0, sizeof(loop_ret));
                    SymTable body_st;
                    sym_table_init(&body_st);
                    body_st.parent = &loop_st;
                    eval_stmts(n->body, &body_st, ft, ct, prints, &loop_ret);
                    sym_table_free(&body_st);
                    if (loop_ret.has_return) {
                        if (ret) {
                            ret->has_return = 1;
                            ret->return_result = loop_ret.return_result;
                        }
                        break;
                    }
                    if (loop_ret.has_break) break;
                    eval_stmts(n->for_update, &loop_st, ft, ct, prints, NULL);
                }
                sym_table_free(&loop_st);
            }
        } else if (n->type == NODE_IF_STMT) {
            /* Check if any condition in the chain involves runtime variables */
            int has_rt_cond = 0;
            if (g_ir) {
                ASTNode *scan = n;
                while (scan && scan->type == NODE_IF_STMT && scan->if_cond) {
                    if (expr_is_runtime(scan->if_cond, st)) {
                        has_rt_cond = 1;
                        break;
                    }
                    scan = scan->else_body;
                }
            }

            if (has_rt_cond) {
                /* Runtime if/else — compile entire chain to IR */
                flush_prints_to_ir(prints);
                ASTNode *branch = n;
                int end_label = ir_alloc_label(g_ir);

                while (branch && branch->type == NODE_IF_STMT && branch->if_cond) {
                    int cond_vreg = ir_compile_expr(branch->if_cond, st, g_ir);
                    int else_label = ir_alloc_label(g_ir);
                    ir_emit_jz(g_ir, cond_vreg, else_label);

                    SymTable if_st;
                    sym_table_init(&if_st);
                    if_st.parent = st;
                    ir_compile_stmts(branch->if_body, &if_st, g_ir, -1, -1);
                    sym_table_free(&if_st);
                    ir_emit_jmp(g_ir, end_label);

                    ir_emit_label(g_ir, else_label);
                    branch = branch->else_body;
                }

                if (branch && branch->type != NODE_IF_STMT) {
                    SymTable else_st;
                    sym_table_init(&else_st);
                    else_st.parent = st;
                    ir_compile_stmts(branch, &else_st, g_ir, -1, -1);
                    sym_table_free(&else_st);
                }

                ir_emit_label(g_ir, end_label);
            } else {
                /* Compile-time if/else — original path */
                ASTNode *branch = n;
                ASTNode *taken_body = NULL;
                while (branch && branch->type == NODE_IF_STMT && branch->if_cond) {
                    EvalResult cond = eval_expr(branch->if_cond, st);
                    if (cond.type != VAL_BOOL)
                        diag_emit(branch->loc, DIAG_ERROR, "if condition must be a bool, got '%s'", value_type_name(cond.type));
                    if (cond.bool_val) {
                        taken_body = branch->if_body;
                        break;
                    }
                    branch = branch->else_body;
                }
                if (!taken_body && branch && branch->type != NODE_IF_STMT)
                    taken_body = branch;

                if (taken_body) {
                    SymTable if_st;
                    sym_table_init(&if_st);
                    if_st.parent = st;
                    eval_stmts(taken_body, &if_st, ft, ct, prints, ret);
                    for (int j = 0; j < if_st.count; j++) {
                        if (!if_st.syms[j].is_const && !if_st.syms[j].mutated)
                            diag_emit(if_st.syms[j].loc, DIAG_WARNING, "variable '%s' is never mutated, consider using 'const'", if_st.syms[j].name);
                    }
                    sym_table_free(&if_st);
                }
            }
        } else if (n->type == NODE_MATCH_STMT) {
            /* Check if scrutinee involves runtime variables */
            int has_rt_scrutinee = g_ir && expr_is_runtime(n->match_expr, st);

            if (has_rt_scrutinee) {
                /* Runtime match — compile to IR */
                flush_prints_to_ir(prints);
                int scrutinee_vreg = ir_compile_expr(n->match_expr, st, g_ir);
                int end_label = ir_alloc_label(g_ir);

                for (int a = 0; a < n->match_arm_count; a++) {
                    MatchArm *arm = &n->match_arms[a];
                    int next_arm_label = ir_alloc_label(g_ir);

                    if (!arm->is_wildcard) {
                        int pattern_vreg = ir_compile_expr(arm->pattern, st, g_ir);
                        int cmp_vreg = ir_emit_binop(g_ir, IR_CMP_NE, scrutinee_vreg, pattern_vreg);
                        ir_emit_jnz(g_ir, cmp_vreg, next_arm_label);
                    }

                    SymTable match_st;
                    sym_table_init(&match_st);
                    match_st.parent = st;
                    ir_compile_stmts(arm->body, &match_st, g_ir, -1, -1);
                    sym_table_free(&match_st);
                    ir_emit_jmp(g_ir, end_label);

                    ir_emit_label(g_ir, next_arm_label);
                }

                ir_emit_label(g_ir, end_label);
            } else {
                /* Compile-time match — original path */
                EvalResult scrutinee = eval_expr(n->match_expr, st);
                int matched = 0;
                for (int a = 0; a < n->match_arm_count; a++) {
                    MatchArm *arm = &n->match_arms[a];
                    if (arm->is_wildcard) {
                        matched = 1;
                    } else {
                        EvalResult pat = eval_expr(arm->pattern, st);
                        EvalResult cmp = eval_binary(BINOP_EQ, scrutinee, pat, n->loc);
                        if (cmp.bool_val)
                            matched = 1;
                    }
                    if (matched) {
                        SymTable match_st;
                        sym_table_init(&match_st);
                        match_st.parent = st;
                        eval_stmts(arm->body, &match_st, ft, ct, prints, ret);
                        for (int j = 0; j < match_st.count; j++) {
                            if (!match_st.syms[j].is_const && !match_st.syms[j].mutated)
                                diag_emit(match_st.syms[j].loc, DIAG_WARNING, "variable '%s' is never mutated, consider using 'const'", match_st.syms[j].name);
                        }
                        sym_table_free(&match_st);
                        break;
                    }
                }
            }
        }
    }
}

/* ================================================================
 * Built-in string functions
 * ================================================================ */

static int eval_builtin_string_fn(const char *fn_name, SourceLoc call_loc,
                                  int arg_count, char **arg_values,
                                  int *arg_lengths, ValueType *arg_types,
                                  char **ret_value, int *ret_len, ValueType *ret_type) {
    /* len(s) -> int */
    if (strcmp(fn_name, "len") == 0) {
        if (arg_count != 1) diag_emit(call_loc, DIAG_ERROR, "len() expects 1 argument, got %d", arg_count);
        if (arg_types[0] != VAL_STRING) diag_emit(call_loc, DIAG_ERROR, "len() expects a string argument");
        char buf[32];
        int n = snprintf(buf, sizeof(buf), "%d", arg_lengths[0]);
        *ret_value = malloc(n + 1);
        memcpy(*ret_value, buf, n + 1);
        *ret_len = n;
        *ret_type = VAL_INT;
        return 1;
    }
    /* trim(s) -> string */
    if (strcmp(fn_name, "trim") == 0) {
        if (arg_count != 1) diag_emit(call_loc, DIAG_ERROR, "trim() expects 1 argument");
        if (arg_types[0] != VAL_STRING) diag_emit(call_loc, DIAG_ERROR, "trim() expects a string argument");
        const char *s = arg_values[0];
        int slen = arg_lengths[0];
        int start = 0, end = slen;
        while (start < end && (s[start] == ' ' || s[start] == '\t' || s[start] == '\n' || s[start] == '\r')) start++;
        while (end > start && (s[end-1] == ' ' || s[end-1] == '\t' || s[end-1] == '\n' || s[end-1] == '\r')) end--;
        int rlen = end - start;
        *ret_value = malloc(rlen + 1);
        memcpy(*ret_value, s + start, rlen);
        (*ret_value)[rlen] = '\0';
        *ret_len = rlen;
        *ret_type = VAL_STRING;
        return 1;
    }
    /* contains(s, sub) -> bool */
    if (strcmp(fn_name, "contains") == 0) {
        if (arg_count != 2) diag_emit(call_loc, DIAG_ERROR, "contains() expects 2 arguments");
        if (arg_types[0] != VAL_STRING || arg_types[1] != VAL_STRING)
            diag_emit(call_loc, DIAG_ERROR, "contains() expects string arguments");
        int found = strstr(arg_values[0], arg_values[1]) != NULL;
        *ret_value = malloc(6);
        strcpy(*ret_value, found ? "true" : "false");
        *ret_len = found ? 4 : 5;
        *ret_type = VAL_BOOL;
        return 1;
    }
    /* replace(s, old, new) -> string */
    if (strcmp(fn_name, "replace") == 0) {
        if (arg_count != 3) diag_emit(call_loc, DIAG_ERROR, "replace() expects 3 arguments");
        if (arg_types[0] != VAL_STRING || arg_types[1] != VAL_STRING || arg_types[2] != VAL_STRING)
            diag_emit(call_loc, DIAG_ERROR, "replace() expects string arguments");
        const char *s = arg_values[0];
        const char *old = arg_values[1];
        const char *new_str = arg_values[2];
        int old_len = arg_lengths[1];
        int new_len = arg_lengths[2];
        if (old_len == 0) {
            /* Replace empty string: return original */
            *ret_value = malloc(arg_lengths[0] + 1);
            memcpy(*ret_value, s, arg_lengths[0] + 1);
            *ret_len = arg_lengths[0];
            *ret_type = VAL_STRING;
            return 1;
        }
        int cap = arg_lengths[0] * 2 + 1;
        char *result = malloc(cap);
        int rpos = 0;
        const char *p = s;
        while (*p) {
            const char *found = strstr(p, old);
            if (!found) {
                int remain = (int)strlen(p);
                while (rpos + remain + 1 > cap) { cap *= 2; result = realloc(result, cap); }
                memcpy(result + rpos, p, remain);
                rpos += remain;
                break;
            }
            int seg = (int)(found - p);
            while (rpos + seg + new_len + 1 > cap) { cap *= 2; result = realloc(result, cap); }
            memcpy(result + rpos, p, seg);
            rpos += seg;
            memcpy(result + rpos, new_str, new_len);
            rpos += new_len;
            p = found + old_len;
        }
        result[rpos] = '\0';
        *ret_value = result;
        *ret_len = rpos;
        *ret_type = VAL_STRING;
        return 1;
    }
    /* to_upper(s) -> string */
    if (strcmp(fn_name, "to_upper") == 0) {
        if (arg_count != 1) diag_emit(call_loc, DIAG_ERROR, "to_upper() expects 1 argument");
        if (arg_types[0] != VAL_STRING) diag_emit(call_loc, DIAG_ERROR, "to_upper() expects a string argument");
        int slen = arg_lengths[0];
        char *r = malloc(slen + 1);
        for (int i = 0; i < slen; i++) {
            char c = arg_values[0][i];
            r[i] = (c >= 'a' && c <= 'z') ? c - 32 : c;
        }
        r[slen] = '\0';
        *ret_value = r;
        *ret_len = slen;
        *ret_type = VAL_STRING;
        return 1;
    }
    /* to_lower(s) -> string */
    if (strcmp(fn_name, "to_lower") == 0) {
        if (arg_count != 1) diag_emit(call_loc, DIAG_ERROR, "to_lower() expects 1 argument");
        if (arg_types[0] != VAL_STRING) diag_emit(call_loc, DIAG_ERROR, "to_lower() expects a string argument");
        int slen = arg_lengths[0];
        char *r = malloc(slen + 1);
        for (int i = 0; i < slen; i++) {
            char c = arg_values[0][i];
            r[i] = (c >= 'A' && c <= 'Z') ? c + 32 : c;
        }
        r[slen] = '\0';
        *ret_value = r;
        *ret_len = slen;
        *ret_type = VAL_STRING;
        return 1;
    }
    /* starts_with(s, prefix) -> bool */
    if (strcmp(fn_name, "starts_with") == 0) {
        if (arg_count != 2) diag_emit(call_loc, DIAG_ERROR, "starts_with() expects 2 arguments");
        if (arg_types[0] != VAL_STRING || arg_types[1] != VAL_STRING)
            diag_emit(call_loc, DIAG_ERROR, "starts_with() expects string arguments");
        int slen = arg_lengths[0], plen = arg_lengths[1];
        int match = (plen <= slen && memcmp(arg_values[0], arg_values[1], plen) == 0);
        *ret_value = malloc(6);
        strcpy(*ret_value, match ? "true" : "false");
        *ret_len = match ? 4 : 5;
        *ret_type = VAL_BOOL;
        return 1;
    }
    /* ends_with(s, suffix) -> bool */
    if (strcmp(fn_name, "ends_with") == 0) {
        if (arg_count != 2) diag_emit(call_loc, DIAG_ERROR, "ends_with() expects 2 arguments");
        if (arg_types[0] != VAL_STRING || arg_types[1] != VAL_STRING)
            diag_emit(call_loc, DIAG_ERROR, "ends_with() expects string arguments");
        int slen = arg_lengths[0], suflen = arg_lengths[1];
        int match = (suflen <= slen && memcmp(arg_values[0] + slen - suflen, arg_values[1], suflen) == 0);
        *ret_value = malloc(6);
        strcpy(*ret_value, match ? "true" : "false");
        *ret_len = match ? 4 : 5;
        *ret_type = VAL_BOOL;
        return 1;
    }
    /* index_of(s, sub) -> int */
    if (strcmp(fn_name, "index_of") == 0) {
        if (arg_count != 2) diag_emit(call_loc, DIAG_ERROR, "index_of() expects 2 arguments");
        if (arg_types[0] != VAL_STRING || arg_types[1] != VAL_STRING)
            diag_emit(call_loc, DIAG_ERROR, "index_of() expects string arguments");
        const char *found = strstr(arg_values[0], arg_values[1]);
        long idx = found ? (long)(found - arg_values[0]) : -1;
        char buf[32];
        int n = snprintf(buf, sizeof(buf), "%ld", idx);
        *ret_value = malloc(n + 1);
        memcpy(*ret_value, buf, n + 1);
        *ret_len = n;
        *ret_type = VAL_INT;
        return 1;
    }
    /* char_at(s, i) -> string */
    if (strcmp(fn_name, "char_at") == 0) {
        if (arg_count != 2) diag_emit(call_loc, DIAG_ERROR, "char_at() expects 2 arguments");
        if (arg_types[0] != VAL_STRING) diag_emit(call_loc, DIAG_ERROR, "char_at() first argument must be a string");
        if (arg_types[1] != VAL_INT) diag_emit(call_loc, DIAG_ERROR, "char_at() second argument must be an int");
        long idx = atol(arg_values[1]);
        int slen = arg_lengths[0];
        if (idx < 0) idx += slen;
        if (idx < 0 || idx >= slen)
            diag_emit(call_loc, DIAG_ERROR, "char_at() index %ld out of range (length %d)", idx, slen);
        *ret_value = malloc(2);
        (*ret_value)[0] = arg_values[0][idx];
        (*ret_value)[1] = '\0';
        *ret_len = 1;
        *ret_type = VAL_STRING;
        return 1;
    }
    /* substr(s, start, end) -> string */
    if (strcmp(fn_name, "substr") == 0) {
        if (arg_count != 3) diag_emit(call_loc, DIAG_ERROR, "substr() expects 3 arguments");
        if (arg_types[0] != VAL_STRING) diag_emit(call_loc, DIAG_ERROR, "substr() first argument must be a string");
        if (arg_types[1] != VAL_INT || arg_types[2] != VAL_INT)
            diag_emit(call_loc, DIAG_ERROR, "substr() start and end must be int");
        long s = atol(arg_values[1]);
        long e = atol(arg_values[2]);
        int slen = arg_lengths[0];
        if (s < 0) s += slen;
        if (e < 0) e += slen;
        if (s < 0) s = 0;
        if (e > slen) e = slen;
        if (s > e) s = e;
        int rlen = (int)(e - s);
        *ret_value = malloc(rlen + 1);
        memcpy(*ret_value, arg_values[0] + s, rlen);
        (*ret_value)[rlen] = '\0';
        *ret_len = rlen;
        *ret_type = VAL_STRING;
        return 1;
    }
    return 0; /* not a built-in */
}

/* ================================================================
 * Built-in array functions (EvalResult-based)
 * ================================================================ */

static EvalResult eval_builtin_array_fn(const char *fn_name, SourceLoc call_loc,
                                        int arg_count, EvalResult *args) {
    EvalResult r;
    memset(&r, 0, sizeof(r));

    /* len(arr) -> int */
    if (strcmp(fn_name, "len") == 0) {
        if (arg_count != 1) diag_emit(call_loc, DIAG_ERROR, "len() expects 1 argument, got %d", arg_count);
        if (args[0].type != VAL_ARRAY) diag_emit(call_loc, DIAG_ERROR, "len() expects an array argument");
        r.type = VAL_INT;
        r.int_val = args[0].arr_val ? args[0].arr_val->count : 0;
        return r;
    }
    /* contains(arr, elem) -> bool */
    if (strcmp(fn_name, "contains") == 0) {
        if (arg_count != 2) diag_emit(call_loc, DIAG_ERROR, "contains() expects 2 arguments");
        if (args[0].type != VAL_ARRAY) diag_emit(call_loc, DIAG_ERROR, "contains() first argument must be an array");
        ArrayData *arr = args[0].arr_val;
        r.type = VAL_BOOL;
        r.bool_val = 0;
        if (arr) {
            for (int i = 0; i < arr->count; i++) {
                EvalResult cmp = eval_binary(BINOP_EQ, arr->elements[i], args[1], call_loc);
                if (cmp.bool_val) { r.bool_val = 1; break; }
            }
        }
        return r;
    }
    /* index_of(arr, elem) -> int */
    if (strcmp(fn_name, "index_of") == 0) {
        if (arg_count != 2) diag_emit(call_loc, DIAG_ERROR, "index_of() expects 2 arguments");
        if (args[0].type != VAL_ARRAY) diag_emit(call_loc, DIAG_ERROR, "index_of() first argument must be an array");
        ArrayData *arr = args[0].arr_val;
        r.type = VAL_INT;
        r.int_val = -1;
        if (arr) {
            for (int i = 0; i < arr->count; i++) {
                EvalResult cmp = eval_binary(BINOP_EQ, arr->elements[i], args[1], call_loc);
                if (cmp.bool_val) { r.int_val = i; break; }
            }
        }
        return r;
    }
    /* push(arr, elem) -> Array<T> */
    if (strcmp(fn_name, "push") == 0) {
        if (arg_count != 2) diag_emit(call_loc, DIAG_ERROR, "push() expects 2 arguments");
        if (args[0].type != VAL_ARRAY) diag_emit(call_loc, DIAG_ERROR, "push() first argument must be an array");
        ArrayData *old = args[0].arr_val;
        int old_count = old ? old->count : 0;
        ValueType et = old && old->elem_type != VAL_VOID ? old->elem_type : args[1].type;
        if (old_count > 0 && args[1].type != et)
            diag_emit(call_loc, DIAG_ERROR, "push() element type '%s' does not match array element type '%s'",
                      value_type_name(args[1].type), value_type_name(et));
        ArrayData *new_arr = malloc(sizeof(ArrayData));
        new_arr->count = old_count + 1;
        new_arr->elem_type = et;
        new_arr->elements = malloc(new_arr->count * sizeof(EvalResult));
        if (old_count > 0)
            memcpy(new_arr->elements, old->elements, old_count * sizeof(EvalResult));
        new_arr->elements[old_count] = args[1];
        r.type = VAL_ARRAY;
        r.arr_val = new_arr;
        return r;
    }
    /* pop(arr) -> Array<T> (removes last element) */
    if (strcmp(fn_name, "pop") == 0) {
        if (arg_count != 1) diag_emit(call_loc, DIAG_ERROR, "pop() expects 1 argument");
        if (args[0].type != VAL_ARRAY) diag_emit(call_loc, DIAG_ERROR, "pop() expects an array argument");
        ArrayData *old = args[0].arr_val;
        if (!old || old->count == 0)
            diag_emit(call_loc, DIAG_ERROR, "pop() on empty array");
        ArrayData *new_arr = malloc(sizeof(ArrayData));
        new_arr->count = old->count - 1;
        new_arr->elem_type = old->elem_type;
        new_arr->elements = malloc((new_arr->count > 0 ? new_arr->count : 1) * sizeof(EvalResult));
        if (new_arr->count > 0)
            memcpy(new_arr->elements, old->elements, new_arr->count * sizeof(EvalResult));
        r.type = VAL_ARRAY;
        r.arr_val = new_arr;
        return r;
    }
    /* shift(arr) -> Array<T> (removes first element) */
    if (strcmp(fn_name, "shift") == 0) {
        if (arg_count != 1) diag_emit(call_loc, DIAG_ERROR, "shift() expects 1 argument");
        if (args[0].type != VAL_ARRAY) diag_emit(call_loc, DIAG_ERROR, "shift() expects an array argument");
        ArrayData *old = args[0].arr_val;
        if (!old || old->count == 0)
            diag_emit(call_loc, DIAG_ERROR, "shift() on empty array");
        ArrayData *new_arr = malloc(sizeof(ArrayData));
        new_arr->count = old->count - 1;
        new_arr->elem_type = old->elem_type;
        new_arr->elements = malloc((new_arr->count > 0 ? new_arr->count : 1) * sizeof(EvalResult));
        if (new_arr->count > 0)
            memcpy(new_arr->elements, old->elements + 1, new_arr->count * sizeof(EvalResult));
        r.type = VAL_ARRAY;
        r.arr_val = new_arr;
        return r;
    }
    /* concat(arr1, arr2) -> Array<T> */
    if (strcmp(fn_name, "concat") == 0) {
        if (arg_count != 2) diag_emit(call_loc, DIAG_ERROR, "concat() expects 2 arguments");
        if (args[0].type != VAL_ARRAY || args[1].type != VAL_ARRAY)
            diag_emit(call_loc, DIAG_ERROR, "concat() expects two array arguments");
        ArrayData *a = args[0].arr_val;
        ArrayData *b = args[1].arr_val;
        int ac = a ? a->count : 0;
        int bc = b ? b->count : 0;
        ArrayData *new_arr = malloc(sizeof(ArrayData));
        new_arr->count = ac + bc;
        new_arr->elem_type = a && a->elem_type != VAL_VOID ? a->elem_type : (b ? b->elem_type : VAL_VOID);
        new_arr->elements = malloc((new_arr->count > 0 ? new_arr->count : 1) * sizeof(EvalResult));
        if (ac > 0) memcpy(new_arr->elements, a->elements, ac * sizeof(EvalResult));
        if (bc > 0) memcpy(new_arr->elements + ac, b->elements, bc * sizeof(EvalResult));
        r.type = VAL_ARRAY;
        r.arr_val = new_arr;
        return r;
    }
    /* reverse(arr) -> Array<T> */
    if (strcmp(fn_name, "reverse") == 0) {
        if (arg_count != 1) diag_emit(call_loc, DIAG_ERROR, "reverse() expects 1 argument");
        if (args[0].type != VAL_ARRAY) diag_emit(call_loc, DIAG_ERROR, "reverse() expects an array argument");
        ArrayData *old = args[0].arr_val;
        int cnt = old ? old->count : 0;
        ArrayData *new_arr = malloc(sizeof(ArrayData));
        new_arr->count = cnt;
        new_arr->elem_type = old ? old->elem_type : VAL_VOID;
        new_arr->elements = malloc((cnt > 0 ? cnt : 1) * sizeof(EvalResult));
        for (int i = 0; i < cnt; i++)
            new_arr->elements[i] = old->elements[cnt - 1 - i];
        r.type = VAL_ARRAY;
        r.arr_val = new_arr;
        return r;
    }
    /* sort(arr) -> Array<T> (ascending, insertion sort) */
    if (strcmp(fn_name, "sort") == 0) {
        if (arg_count != 1) diag_emit(call_loc, DIAG_ERROR, "sort() expects 1 argument");
        if (args[0].type != VAL_ARRAY) diag_emit(call_loc, DIAG_ERROR, "sort() expects an array argument");
        ArrayData *old = args[0].arr_val;
        int cnt = old ? old->count : 0;
        ArrayData *new_arr = malloc(sizeof(ArrayData));
        new_arr->count = cnt;
        new_arr->elem_type = old ? old->elem_type : VAL_VOID;
        new_arr->elements = malloc((cnt > 0 ? cnt : 1) * sizeof(EvalResult));
        if (cnt > 0)
            memcpy(new_arr->elements, old->elements, cnt * sizeof(EvalResult));
        /* Insertion sort */
        for (int i = 1; i < cnt; i++) {
            EvalResult key = new_arr->elements[i];
            int j = i - 1;
            while (j >= 0) {
                EvalResult cmp = eval_binary(BINOP_GT, new_arr->elements[j], key, call_loc);
                if (!cmp.bool_val) break;
                new_arr->elements[j + 1] = new_arr->elements[j];
                j--;
            }
            new_arr->elements[j + 1] = key;
        }
        r.type = VAL_ARRAY;
        r.arr_val = new_arr;
        return r;
    }
    /* join(arr, sep) -> string */
    if (strcmp(fn_name, "join") == 0) {
        if (arg_count != 2) diag_emit(call_loc, DIAG_ERROR, "join() expects 2 arguments");
        if (args[0].type != VAL_ARRAY) diag_emit(call_loc, DIAG_ERROR, "join() first argument must be an array");
        if (args[1].type != VAL_STRING) diag_emit(call_loc, DIAG_ERROR, "join() second argument must be a string");
        ArrayData *arr = args[0].arr_val;
        const char *sep = args[1].str_val;
        int sep_len = args[1].str_len;
        int cnt = arr ? arr->count : 0;
        int cap = 256;
        char *s = malloc(cap);
        int pos = 0;
        for (int i = 0; i < cnt; i++) {
            if (i > 0) {
                while (pos + sep_len + 1 > cap) { cap *= 2; s = realloc(s, cap); }
                memcpy(s + pos, sep, sep_len);
                pos += sep_len;
            }
            int elen;
            char *estr = eval_to_string(&arr->elements[i], &elen);
            while (pos + elen + 1 > cap) { cap *= 2; s = realloc(s, cap); }
            memcpy(s + pos, estr, elen);
            pos += elen;
            free(estr);
        }
        s[pos] = '\0';
        r.type = VAL_STRING;
        r.str_val = s;
        r.str_len = pos;
        return r;
    }
    /* remove(arr, index) -> Array<T> */
    if (strcmp(fn_name, "remove") == 0) {
        if (arg_count != 2) diag_emit(call_loc, DIAG_ERROR, "remove() expects 2 arguments");
        if (args[0].type != VAL_ARRAY) diag_emit(call_loc, DIAG_ERROR, "remove() first argument must be an array");
        if (args[1].type != VAL_INT) diag_emit(call_loc, DIAG_ERROR, "remove() second argument must be an int");
        ArrayData *old = args[0].arr_val;
        int cnt = old ? old->count : 0;
        long idx = args[1].int_val;
        if (idx < 0) idx += cnt;
        if (idx < 0 || idx >= cnt)
            diag_emit(call_loc, DIAG_ERROR, "remove() index %ld out of range (length %d)", args[1].int_val, cnt);
        ArrayData *new_arr = malloc(sizeof(ArrayData));
        new_arr->count = cnt - 1;
        new_arr->elem_type = old->elem_type;
        new_arr->elements = malloc((new_arr->count > 0 ? new_arr->count : 1) * sizeof(EvalResult));
        if (idx > 0)
            memcpy(new_arr->elements, old->elements, idx * sizeof(EvalResult));
        if (idx < cnt - 1)
            memcpy(new_arr->elements + idx, old->elements + idx + 1, (cnt - 1 - idx) * sizeof(EvalResult));
        r.type = VAL_ARRAY;
        r.arr_val = new_arr;
        return r;
    }

    /* Not an array built-in */
    r.type = VAL_VOID;
    return r;
}

/* Wrapper: call string built-in with EvalResult args */
static EvalResult eval_builtin_string_fn_eval(const char *fn_name, SourceLoc call_loc,
                                              int arg_count, EvalResult *args) {
    char **av = malloc((arg_count > 0 ? arg_count : 1) * sizeof(char *));
    int *al = malloc((arg_count > 0 ? arg_count : 1) * sizeof(int));
    ValueType *at = malloc((arg_count > 0 ? arg_count : 1) * sizeof(ValueType));
    for (int i = 0; i < arg_count; i++) {
        at[i] = args[i].type;
        av[i] = eval_to_string(&args[i], &al[i]);
    }
    char *rv = NULL; int rl = 0; ValueType rt = VAL_VOID;
    int handled = eval_builtin_string_fn(fn_name, call_loc, arg_count, av, al, at, &rv, &rl, &rt);
    for (int i = 0; i < arg_count; i++) free(av[i]);
    free(av); free(al); free(at);

    EvalResult result;
    memset(&result, 0, sizeof(result));
    if (handled) {
        result.type = rt;
        switch (rt) {
            case VAL_INT:    result.int_val = atol(rv); break;
            case VAL_FLOAT:  result.float_val = atof(rv); break;
            case VAL_STRING: result.str_val = rv; result.str_len = rl; rv = NULL; break;
            case VAL_BOOL:   result.bool_val = (strcmp(rv, "true") == 0); break;
            default: break;
        }
    }
    free(rv);
    return result;
}

/* ================================================================
 * Built-in concurrency functions (EvalResult-based)
 * ================================================================ */

static EvalResult eval_builtin_concurrency_fn(const char *fn_name, SourceLoc call_loc,
                                              int arg_count, EvalResult *args) {
    EvalResult r;
    memset(&r, 0, sizeof(r));

    /* send(ch, value) -> void */
    if (strcmp(fn_name, "send") == 0) {
        if (arg_count != 2)
            diag_emit(call_loc, DIAG_ERROR, "send() expects 2 arguments, got %d", arg_count);
        if (args[0].type != VAL_CHANNEL || !args[0].chan_val)
            diag_emit(call_loc, DIAG_ERROR, "send() first argument must be a channel");
        ChannelData *ch = args[0].chan_val;
        if (args[1].type != ch->elem_type)
            diag_emit(call_loc, DIAG_ERROR, "send() value type '%s' does not match channel element type '%s'",
                      value_type_name(args[1].type), value_type_name(ch->elem_type));
        /* Grow buffer if needed */
        if (ch->count == ch->cap) {
            ch->cap *= 2;
            ch->buffer = realloc(ch->buffer, ch->cap * sizeof(EvalResult));
        }
        ch->buffer[ch->count++] = args[1];
        r.type = VAL_VOID;
        return r;
    }

    /* receive(ch) -> T */
    if (strcmp(fn_name, "receive") == 0) {
        if (arg_count != 1)
            diag_emit(call_loc, DIAG_ERROR, "receive() expects 1 argument, got %d", arg_count);
        if (args[0].type != VAL_CHANNEL || !args[0].chan_val)
            diag_emit(call_loc, DIAG_ERROR, "receive() argument must be a channel");
        ChannelData *ch = args[0].chan_val;
        if (ch->read_pos >= ch->count)
            diag_emit(call_loc, DIAG_ERROR, "receive() on empty channel");
        return ch->buffer[ch->read_pos++];
    }

    r.type = VAL_VOID;
    return r;
}

/* ================================================================
 * Built-in HTTP functions (std/http)
 * ================================================================ */

static EvalResult eval_builtin_http_fn(const char *fn_name, SourceLoc call_loc,
                                       int arg_count, EvalResult *args) {
    EvalResult r;
    memset(&r, 0, sizeof(r));
    r.type = VAL_VOID;

    /* get(path, body) -> void */
    if (strcmp(fn_name, "get") == 0) {
        if (arg_count != 2)
            diag_emit(call_loc, DIAG_ERROR, "get() expects 2 arguments, got %d", arg_count);
        if (args[0].type != VAL_STRING)
            diag_emit(call_loc, DIAG_ERROR, "get() first argument (path) must be a string");
        if (args[1].type != VAL_STRING)
            diag_emit(call_loc, DIAG_ERROR, "get() second argument (body) must be a string");
        if (g_http_route_count == g_http_route_cap) {
            g_http_route_cap = g_http_route_cap ? g_http_route_cap * 2 : 8;
            g_http_routes = realloc(g_http_routes, g_http_route_cap * sizeof(HttpRouteEntry));
        }
        HttpRouteEntry *entry = &g_http_routes[g_http_route_count++];
        entry->method = "GET";
        entry->path = args[0].str_val;
        entry->path_len = args[0].str_len;
        entry->body = args[1].str_val;
        entry->body_len = args[1].str_len;
        return r;
    }

    /* post(path, body) -> void */
    if (strcmp(fn_name, "post") == 0) {
        if (arg_count != 2)
            diag_emit(call_loc, DIAG_ERROR, "post() expects 2 arguments, got %d", arg_count);
        if (args[0].type != VAL_STRING)
            diag_emit(call_loc, DIAG_ERROR, "post() first argument (path) must be a string");
        if (args[1].type != VAL_STRING)
            diag_emit(call_loc, DIAG_ERROR, "post() second argument (body) must be a string");
        if (g_http_route_count == g_http_route_cap) {
            g_http_route_cap = g_http_route_cap ? g_http_route_cap * 2 : 8;
            g_http_routes = realloc(g_http_routes, g_http_route_cap * sizeof(HttpRouteEntry));
        }
        HttpRouteEntry *entry = &g_http_routes[g_http_route_count++];
        entry->method = "POST";
        entry->path = args[0].str_val;
        entry->path_len = args[0].str_len;
        entry->body = args[1].str_val;
        entry->body_len = args[1].str_len;
        return r;
    }

    /* listen(port) -> void */
    if (strcmp(fn_name, "listen") == 0) {
        if (arg_count != 1)
            diag_emit(call_loc, DIAG_ERROR, "listen() expects 1 argument, got %d", arg_count);
        if (args[0].type != VAL_INT)
            diag_emit(call_loc, DIAG_ERROR, "listen() argument (port) must be an int");
        long port = args[0].int_val;
        if (port < 1 || port > 65535)
            diag_emit(call_loc, DIAG_ERROR, "listen() port must be between 1 and 65535, got %ld", port);
        if (g_http_listen_called)
            diag_emit(call_loc, DIAG_ERROR, "listen() can only be called once");
        g_http_listen_port = (int)port;
        g_http_listen_called = 1;
        return r;
    }

    return r;
}

/* ================================================================
 * Built-in net functions (std/net)
 * ================================================================ */

static EvalResult eval_builtin_net_fn(const char *fn_name, SourceLoc call_loc,
                                      int arg_count, EvalResult *args) {
    EvalResult r;
    memset(&r, 0, sizeof(r));
    r.type = VAL_VOID;

    /* tcp_listen(port, response) -> void */
    if (strcmp(fn_name, "tcp_listen") == 0) {
        if (arg_count != 2)
            diag_emit(call_loc, DIAG_ERROR, "tcp_listen() expects 2 arguments, got %d", arg_count);
        if (args[0].type != VAL_INT)
            diag_emit(call_loc, DIAG_ERROR, "tcp_listen() first argument (port) must be an int");
        if (args[1].type != VAL_STRING)
            diag_emit(call_loc, DIAG_ERROR, "tcp_listen() second argument (response) must be a string");
        long port = args[0].int_val;
        if (port < 1 || port > 65535)
            diag_emit(call_loc, DIAG_ERROR, "tcp_listen() port must be between 1 and 65535, got %ld", port);
        if (g_net_mode_set)
            diag_emit(call_loc, DIAG_ERROR, "only one networking mode can be configured per program");
        g_net_config.mode = NET_TCP_LISTEN;
        g_net_config.port = (int)port;
        g_net_config.host = NULL;
        g_net_config.data = args[1].str_val;
        g_net_config.data_len = args[1].str_len;
        g_net_mode_set = 1;
        return r;
    }

    /* tcp_connect(host, port, message) -> void */
    if (strcmp(fn_name, "tcp_connect") == 0) {
        if (arg_count != 3)
            diag_emit(call_loc, DIAG_ERROR, "tcp_connect() expects 3 arguments, got %d", arg_count);
        if (args[0].type != VAL_STRING)
            diag_emit(call_loc, DIAG_ERROR, "tcp_connect() first argument (host) must be a string");
        if (args[1].type != VAL_INT)
            diag_emit(call_loc, DIAG_ERROR, "tcp_connect() second argument (port) must be an int");
        if (args[2].type != VAL_STRING)
            diag_emit(call_loc, DIAG_ERROR, "tcp_connect() third argument (message) must be a string");
        long port = args[1].int_val;
        if (port < 1 || port > 65535)
            diag_emit(call_loc, DIAG_ERROR, "tcp_connect() port must be between 1 and 65535, got %ld", port);
        if (g_net_mode_set)
            diag_emit(call_loc, DIAG_ERROR, "only one networking mode can be configured per program");
        g_net_config.mode = NET_TCP_CONNECT;
        g_net_config.port = (int)port;
        g_net_config.host = args[0].str_val;
        g_net_config.data = args[2].str_val;
        g_net_config.data_len = args[2].str_len;
        g_net_mode_set = 1;
        return r;
    }

    /* udp_listen(port, response) -> void */
    if (strcmp(fn_name, "udp_listen") == 0) {
        if (arg_count != 2)
            diag_emit(call_loc, DIAG_ERROR, "udp_listen() expects 2 arguments, got %d", arg_count);
        if (args[0].type != VAL_INT)
            diag_emit(call_loc, DIAG_ERROR, "udp_listen() first argument (port) must be an int");
        if (args[1].type != VAL_STRING)
            diag_emit(call_loc, DIAG_ERROR, "udp_listen() second argument (response) must be a string");
        long port = args[0].int_val;
        if (port < 1 || port > 65535)
            diag_emit(call_loc, DIAG_ERROR, "udp_listen() port must be between 1 and 65535, got %ld", port);
        if (g_net_mode_set)
            diag_emit(call_loc, DIAG_ERROR, "only one networking mode can be configured per program");
        g_net_config.mode = NET_UDP_LISTEN;
        g_net_config.port = (int)port;
        g_net_config.host = NULL;
        g_net_config.data = args[1].str_val;
        g_net_config.data_len = args[1].str_len;
        g_net_mode_set = 1;
        return r;
    }

    /* udp_send(host, port, message) -> void */
    if (strcmp(fn_name, "udp_send") == 0) {
        if (arg_count != 3)
            diag_emit(call_loc, DIAG_ERROR, "udp_send() expects 3 arguments, got %d", arg_count);
        if (args[0].type != VAL_STRING)
            diag_emit(call_loc, DIAG_ERROR, "udp_send() first argument (host) must be a string");
        if (args[1].type != VAL_INT)
            diag_emit(call_loc, DIAG_ERROR, "udp_send() second argument (port) must be an int");
        if (args[2].type != VAL_STRING)
            diag_emit(call_loc, DIAG_ERROR, "udp_send() third argument (message) must be a string");
        long port = args[1].int_val;
        if (port < 1 || port > 65535)
            diag_emit(call_loc, DIAG_ERROR, "udp_send() port must be between 1 and 65535, got %ld", port);
        if (g_net_mode_set)
            diag_emit(call_loc, DIAG_ERROR, "only one networking mode can be configured per program");
        g_net_config.mode = NET_UDP_SEND;
        g_net_config.port = (int)port;
        g_net_config.host = args[0].str_val;
        g_net_config.data = args[2].str_val;
        g_net_config.data_len = args[2].str_len;
        g_net_mode_set = 1;
        return r;
    }

    /* start() -> void */
    if (strcmp(fn_name, "start") == 0) {
        if (arg_count != 0)
            diag_emit(call_loc, DIAG_ERROR, "start() expects 0 arguments, got %d", arg_count);
        if (!g_net_mode_set)
            diag_emit(call_loc, DIAG_ERROR, "start() called without configuring a networking mode (call tcp_listen, tcp_connect, udp_listen, or udp_send first)");
        if (g_net_start_called)
            diag_emit(call_loc, DIAG_ERROR, "start() can only be called once");
        g_net_start_called = 1;
        return r;
    }

    return r;
}

/* ================================================================
 * Compile-time function evaluation (EvalResult-based)
 * ================================================================ */

static EvalResult evaluate_fn_call(FnTable *ft, ClassTable *ct, SymTable *outer_st,
                                   const char *fn_name, SourceLoc call_loc,
                                   int arg_count,
                                   EvalResult *arg_results,
                                   char **arg_names,
                                   PrintList *prints) {
    EvalResult void_result;
    memset(&void_result, 0, sizeof(void_result));
    void_result.type = VAL_VOID;

    /* User-defined functions take priority over stdlib */
    FnEntry *fn = fn_table_find(ft, fn_name);
    if (!fn) {
        /* Fall back to stdlib built-ins if imported */
        /* Array-only functions */
        if (stdlib_array_fn_is_imported(fn_name)) {
            return eval_builtin_array_fn(fn_name, call_loc, arg_count, arg_results);
        }
        /* Shared functions: dispatch by first arg type */
        if (stdlib_fn_is_imported(fn_name)) {
            if (arg_count > 0 && arg_results[0].type == VAL_ARRAY) {
                return eval_builtin_array_fn(fn_name, call_loc, arg_count, arg_results);
            }
            return eval_builtin_string_fn_eval(fn_name, call_loc, arg_count, arg_results);
        }
        /* Concurrency functions */
        if (stdlib_concurrency_fn_is_imported(fn_name)) {
            return eval_builtin_concurrency_fn(fn_name, call_loc, arg_count, arg_results);
        }
        /* HTTP functions */
        if (stdlib_http_fn_is_imported(fn_name)) {
            return eval_builtin_http_fn(fn_name, call_loc, arg_count, arg_results);
        }
        /* Net functions */
        if (stdlib_net_fn_is_imported(fn_name)) {
            return eval_builtin_net_fn(fn_name, call_loc, arg_count, arg_results);
        }
        diag_emit(call_loc, DIAG_ERROR, "undefined function '%s'", fn_name);
    }

    ASTNode *decl = fn->decl;

    if (arg_count > decl->param_count)
        diag_emit(call_loc, DIAG_ERROR, "function '%s' expects at most %d argument(s), got %d",
                  fn_name, decl->param_count, arg_count);

    EvalResult *final_results = malloc((decl->param_count > 0 ? decl->param_count : 1) * sizeof(EvalResult));
    int *final_filled = calloc(decl->param_count > 0 ? decl->param_count : 1, sizeof(int));

    int has_named = 0;
    if (arg_names) {
        for (int i = 0; i < arg_count; i++) {
            if (arg_names[i]) { has_named = 1; break; }
        }
    }

    if (has_named) {
        int pos_idx = 0;
        for (int i = 0; i < arg_count; i++) {
            if (arg_names && arg_names[i]) continue;
            if (pos_idx >= decl->param_count)
                diag_emit(call_loc, DIAG_ERROR, "too many positional arguments for function '%s'", fn_name);
            final_results[pos_idx] = arg_results[i];
            final_filled[pos_idx] = 1;
            pos_idx++;
        }
        for (int i = 0; i < arg_count; i++) {
            if (!arg_names || !arg_names[i]) continue;
            int found = 0;
            for (int p = 0; p < decl->param_count; p++) {
                if (strcmp(arg_names[i], decl->params[p].name) == 0) {
                    if (final_filled[p])
                        diag_emit(call_loc, DIAG_ERROR, "duplicate argument for parameter '%s' in function '%s'",
                                  arg_names[i], fn_name);
                    final_results[p] = arg_results[i];
                    final_filled[p] = 1;
                    found = 1;
                    break;
                }
            }
            if (!found)
                diag_emit(call_loc, DIAG_ERROR, "unknown parameter '%s' in function '%s'",
                          arg_names[i], fn_name);
        }
    } else {
        for (int i = 0; i < arg_count; i++) {
            final_results[i] = arg_results[i];
            final_filled[i] = 1;
        }
    }

    /* Fill defaults for missing params */
    for (int i = 0; i < decl->param_count; i++) {
        if (!final_filled[i]) {
            if (!decl->params[i].has_default)
                diag_emit(call_loc, DIAG_ERROR, "missing argument for required parameter '%s' in function '%s'",
                          decl->params[i].name, fn_name);
            memset(&final_results[i], 0, sizeof(EvalResult));
            final_results[i].type = decl->params[i].type;
            switch (decl->params[i].type) {
                case VAL_INT:    final_results[i].int_val = atol(decl->params[i].default_value); break;
                case VAL_FLOAT:  final_results[i].float_val = atof(decl->params[i].default_value); break;
                case VAL_STRING: final_results[i].str_val = decl->params[i].default_value;
                                 final_results[i].str_len = decl->params[i].default_value_len; break;
                case VAL_BOOL:   final_results[i].bool_val = (strcmp(decl->params[i].default_value, "true") == 0); break;
                default: break;
            }
            final_filled[i] = 1;
        }
    }

    /* Type check params */
    for (int i = 0; i < decl->param_count; i++) {
        if (final_results[i].type != decl->params[i].type)
            diag_emit(call_loc, DIAG_ERROR, "function '%s' parameter '%s' expects '%s', got '%s'",
                      fn_name, decl->params[i].name,
                      value_type_name(decl->params[i].type),
                      value_type_name(final_results[i].type));
    }

    /* Recursion depth limit */
    if (ft->eval_count >= 1000)
        diag_emit(call_loc, DIAG_ERROR, "recursion depth limit exceeded (1000) in function '%s'", fn_name);
    if (ft->eval_count == ft->eval_cap) {
        ft->eval_cap *= 2;
        ft->evaluating = realloc(ft->evaluating, ft->eval_cap * sizeof(char *));
    }
    ft->evaluating[ft->eval_count++] = (char *)fn_name;

    /* Build local symbol table with parent chain to outer scope */
    SymTable local_st;
    sym_table_init(&local_st);
    local_st.parent = outer_st;

    for (int i = 0; i < decl->param_count; i++) {
        sym_add(&local_st, decl->params[i].name, final_results[i], 1, decl->loc);
    }

    ReturnCtx ret_ctx;
    memset(&ret_ctx, 0, sizeof(ret_ctx));

    eval_stmts(decl->body, &local_st, ft, ct, prints, &ret_ctx);

    ft->eval_count--;

    if (decl->has_return_type && !ret_ctx.has_return)
        diag_emit(decl->loc, DIAG_ERROR, "function '%s' must return a value of type '%s'",
                  fn_name, value_type_name(decl->return_type));

    if (ret_ctx.has_return && decl->has_return_type && ret_ctx.return_result.type != decl->return_type)
        diag_emit(call_loc, DIAG_ERROR, "function '%s' returns '%s', expected '%s'",
                  fn_name, value_type_name(ret_ctx.return_result.type), value_type_name(decl->return_type));

    EvalResult result = void_result;
    if (ret_ctx.has_return)
        result = ret_ctx.return_result;

    sym_table_free(&local_st);
    free(final_results); free(final_filled);
    return result;
}

/* ================================================================
 * Main codegen entry point
 * ================================================================ */

/* Helper: collect fn/class/enum declarations from an AST into tables */
static void collect_declarations(ASTNode *ast, FnTable *fn_table, ClassTable *class_table, EnumTable *enum_table) {
    for (ASTNode *n = ast; n; n = n->next) {
        if (n->type == NODE_FN_DECL) {
            if (!fn_table_find(fn_table, n->fn_name))
                fn_table_add(fn_table, n->fn_name, n);
        }
        if (n->type == NODE_CLASS_DECL) {
            if (!class_table_find(class_table, n->class_name)) {
                int total_fields = 0;
                char **all_names = NULL;
                ValueType *all_types = NULL;

                if (n->parent_class_name) {
                    ClassDef *parent = class_table_find(class_table, n->parent_class_name);
                    if (parent) {
                        total_fields = parent->field_count + n->class_field_count;
                        all_names = malloc(total_fields * sizeof(char *));
                        all_types = malloc(total_fields * sizeof(ValueType));
                        for (int i = 0; i < parent->field_count; i++) {
                            all_names[i] = parent->field_names[i];
                            all_types[i] = parent->field_types[i];
                        }
                        for (int i = 0; i < n->class_field_count; i++) {
                            all_names[parent->field_count + i] = n->class_fields[i].name;
                            all_types[parent->field_count + i] = n->class_fields[i].type;
                        }
                    } else {
                        diag_emit(n->loc, DIAG_ERROR, "undefined parent class '%s'", n->parent_class_name);
                    }
                } else {
                    total_fields = n->class_field_count;
                    all_names = malloc(total_fields * sizeof(char *));
                    all_types = malloc(total_fields * sizeof(ValueType));
                    for (int i = 0; i < n->class_field_count; i++) {
                        all_names[i] = n->class_fields[i].name;
                        all_types[i] = n->class_fields[i].type;
                    }
                }

                if (class_table->count == class_table->cap) {
                    class_table->cap *= 2;
                    class_table->entries = realloc(class_table->entries, class_table->cap * sizeof(ClassDef));
                }
                ClassDef *cd = &class_table->entries[class_table->count++];
                cd->name = n->class_name;
                cd->parent_name = n->parent_class_name;
                cd->field_names = all_names;
                cd->field_types = all_types;
                cd->field_count = total_fields;
                cd->methods = n->class_methods;
                cd->loc = n->loc;
            }
        }
        if (n->type == NODE_ENUM_DECL) {
            if (!enum_table_find(enum_table, n->enum_name)) {
                if (enum_table->count == enum_table->cap) {
                    enum_table->cap *= 2;
                    enum_table->entries = realloc(enum_table->entries, enum_table->cap * sizeof(EnumDef));
                }
                EnumDef *ed = &enum_table->entries[enum_table->count++];
                ed->name = n->enum_name;
                ed->variant_count = n->enum_variant_count;
                ed->variant_names = malloc(ed->variant_count * sizeof(char *));
                ed->variant_values = malloc(ed->variant_count * sizeof(long));
                for (int i = 0; i < ed->variant_count; i++) {
                    ed->variant_names[i] = n->enum_variants[i].name;
                    ed->variant_values[i] = n->enum_variants[i].value;
                }
                ed->loc = n->loc;
            }
        }
    }
}

/* Imported variable entry — stores evaluated values from imported modules */
typedef struct {
    char *name;
    EvalResult val;
    int is_const;
} ImportedVar;

/* Process all imports for an AST, recursively handling transitive imports.
   source_file: absolute path of the file being processed.
   Populates fn_table, class_table, and imp_vars with imported symbols. */
static void process_imports(ASTNode *ast, const char *source_file,
                            FnTable *fn_table, ClassTable *class_table,
                            EnumTable *enum_table,
                            ImportedVar **imp_vars, int *imp_var_count, int *imp_var_cap) {
    for (ASTNode *n = ast; n; n = n->next) {
        if (n->type != NODE_IMPORT) continue;

        /* Handle stdlib modules (resolved in-compiler, no file needed) */
        if (strcmp(n->import_path, "std/string") == 0) {
            for (int i = 0; i < n->import_name_count; i++) {
                const char *name = n->import_names[i];
                if (stdlib_fn_index(name) < 0)
                    diag_emit(n->loc, DIAG_ERROR, "'%s' not found in module '%s'",
                              name, n->import_path);
                stdlib_fn_import(name);
            }
            continue;
        }
        if (strcmp(n->import_path, "std/array") == 0) {
            for (int i = 0; i < n->import_name_count; i++) {
                const char *name = n->import_names[i];
                /* Shared functions (also in std/string): len, contains, index_of */
                if (stdlib_fn_index(name) >= 0) {
                    stdlib_fn_import(name);
                } else if (stdlib_array_fn_index(name) >= 0) {
                    stdlib_array_fn_import(name);
                } else {
                    diag_emit(n->loc, DIAG_ERROR, "'%s' not found in module '%s'",
                              name, n->import_path);
                }
            }
            continue;
        }
        if (strcmp(n->import_path, "std/concurrency") == 0) {
            for (int i = 0; i < n->import_name_count; i++) {
                const char *name = n->import_names[i];
                if (stdlib_concurrency_fn_index(name) >= 0) {
                    stdlib_concurrency_fn_import(name);
                } else {
                    diag_emit(n->loc, DIAG_ERROR, "'%s' not found in module '%s'",
                              name, n->import_path);
                }
            }
            continue;
        }
        if (strcmp(n->import_path, "std/http") == 0) {
            for (int i = 0; i < n->import_name_count; i++) {
                const char *name = n->import_names[i];
                if (stdlib_http_fn_index(name) >= 0) {
                    stdlib_http_fn_import(name);
                } else {
                    diag_emit(n->loc, DIAG_ERROR, "'%s' not found in module '%s'",
                              name, n->import_path);
                }
            }
            continue;
        }
        if (strcmp(n->import_path, "std/net") == 0) {
            for (int i = 0; i < n->import_name_count; i++) {
                const char *name = n->import_names[i];
                if (stdlib_net_fn_index(name) >= 0) {
                    stdlib_net_fn_import(name);
                } else {
                    diag_emit(n->loc, DIAG_ERROR, "'%s' not found in module '%s'",
                              name, n->import_path);
                }
            }
            continue;
        }

        ASTNode *imported_ast = NULL;
        const char *imported_source = NULL;
        const char *imported_filename = NULL;

        if (import_resolve(source_file, n->import_path, n->loc,
                           &imported_ast, &imported_source, &imported_filename) != 0) {
            continue;
        }

        /* Save/restore diagnostic context for the imported file */
        DiagContext saved_ctx = diag_save();
        diag_init(imported_filename, imported_source);

        /* Build temporary tables for the imported file */
        FnTable imp_ft;
        fn_table_init(&imp_ft);
        ClassTable imp_ct;
        class_table_init(&imp_ct);
        EnumTable imp_et;
        enum_table_init(&imp_et);

        /* Recursively process the imported file's own imports first */
        ImportedVar *nested_vars = malloc(8 * sizeof(ImportedVar));
        int nested_var_count = 0, nested_var_cap = 8;
        import_push_file(imported_filename);
        process_imports(imported_ast, imported_filename, &imp_ft, &imp_ct, &imp_et,
                        &nested_vars, &nested_var_count, &nested_var_cap);
        import_pop_file();

        collect_declarations(imported_ast, &imp_ft, &imp_ct, &imp_et);

        /* Evaluate the imported file to resolve its top-level symbols */
        SymTable imp_st;
        sym_table_init(&imp_st);

        /* Add nested imported variables */
        for (int i = 0; i < nested_var_count; i++) {
            sym_add(&imp_st, nested_vars[i].name, nested_vars[i].val,
                    nested_vars[i].is_const, (SourceLoc){0, 0});
            imp_st.syms[imp_st.count - 1].mutated = 1;
        }

        PrintList imp_prints;
        print_list_init(&imp_prints);

        FnTable *save_ft = g_ft;
        ClassTable *save_ct = g_ct;
        EnumTable *save_et = g_et;
        PrintList *save_prints = g_prints;
        g_ft = &imp_ft;
        g_ct = &imp_ct;
        g_et = &imp_et;
        g_prints = &imp_prints;

        eval_stmts(imported_ast, &imp_st, &imp_ft, &imp_ct, &imp_prints, NULL);

        g_ft = save_ft;
        g_ct = save_ct;
        g_et = save_et;
        g_prints = save_prints;

        diag_restore(saved_ctx);

        /* Copy requested symbols into the caller's tables */
        for (int i = 0; i < n->import_name_count; i++) {
            const char *name = n->import_names[i];
            int found = 0;

            for (ASTNode *imp_n = imported_ast; imp_n; imp_n = imp_n->next) {
                if (imp_n->type == NODE_FN_DECL && strcmp(imp_n->fn_name, name) == 0) {
                    if (!imp_n->is_pub)
                        diag_emit(n->loc, DIAG_ERROR, "'%s' is not public in module '%s'",
                                  name, n->import_path);
                    if (fn_table_find(fn_table, name))
                        diag_emit(n->loc, DIAG_ERROR, "duplicate symbol '%s' from import", name);
                    fn_table_add(fn_table, imp_n->fn_name, imp_n);
                    found = 1;
                    break;
                }
                if (imp_n->type == NODE_CLASS_DECL && strcmp(imp_n->class_name, name) == 0) {
                    if (!imp_n->is_pub)
                        diag_emit(n->loc, DIAG_ERROR, "'%s' is not public in module '%s'",
                                  name, n->import_path);
                    ClassDef *imp_cd = class_table_find(&imp_ct, name);
                    if (imp_cd && !class_table_find(class_table, name)) {
                        if (class_table->count == class_table->cap) {
                            class_table->cap *= 2;
                            class_table->entries = realloc(class_table->entries, class_table->cap * sizeof(ClassDef));
                        }
                        class_table->entries[class_table->count++] = *imp_cd;
                    }
                    found = 1;
                    break;
                }
                if (imp_n->type == NODE_ENUM_DECL && strcmp(imp_n->enum_name, name) == 0) {
                    if (!imp_n->is_pub)
                        diag_emit(n->loc, DIAG_ERROR, "'%s' is not public in module '%s'",
                                  name, n->import_path);
                    EnumDef *imp_ed = enum_table_find(&imp_et, name);
                    if (imp_ed && !enum_table_find(enum_table, name)) {
                        if (enum_table->count == enum_table->cap) {
                            enum_table->cap *= 2;
                            enum_table->entries = realloc(enum_table->entries, enum_table->cap * sizeof(EnumDef));
                        }
                        enum_table->entries[enum_table->count++] = *imp_ed;
                    }
                    found = 1;
                    break;
                }
            }

            if (!found) {
                for (ASTNode *imp_n = imported_ast; imp_n; imp_n = imp_n->next) {
                    if (imp_n->type == NODE_VAR_DECL && strcmp(imp_n->var_name, name) == 0) {
                        if (!imp_n->is_pub)
                            diag_emit(n->loc, DIAG_ERROR, "'%s' is not public in module '%s'",
                                      name, n->import_path);
                        Symbol *sym = sym_find(&imp_st, name);
                        if (sym) {
                            if (*imp_var_count == *imp_var_cap) {
                                *imp_var_cap *= 2;
                                *imp_vars = realloc(*imp_vars, *imp_var_cap * sizeof(ImportedVar));
                            }
                            (*imp_vars)[*imp_var_count].name = strdup(name);
                            (*imp_vars)[*imp_var_count].val = sym->val;
                            (*imp_vars)[*imp_var_count].is_const = sym->is_const;
                            (*imp_var_count)++;
                        }
                        found = 1;
                        break;
                    }
                }
            }

            if (!found)
                diag_emit(n->loc, DIAG_ERROR, "'%s' not found in module '%s'",
                          name, n->import_path);
        }

        /* Clean up nested vars names (values are copied) */
        for (int i = 0; i < nested_var_count; i++)
            free(nested_vars[i].name);
        free(nested_vars);

        print_list_free(&imp_prints);
        fn_table_free(&imp_ft);
        class_table_free(&imp_ct);
        enum_table_free(&imp_et);
        sym_table_free(&imp_st);
    }
}

int codegen(ASTNode *ast, const char *output_path, const char *source_file) {
    stdlib_reset();

    /* Pass 0: process imports */
    if (source_file) {
        char *source_copy = strdup(source_file);
        char *dir = dirname(source_copy);
        import_init(dir);
        free(source_copy);
    }

    /* First pass: collect function, class, and enum declarations */
    FnTable fn_table;
    fn_table_init(&fn_table);

    ClassTable class_table;
    class_table_init(&class_table);

    EnumTable enum_table;
    enum_table_init(&enum_table);

    /* Imported variables list */
    int imp_var_cap = 8;
    int imp_var_count = 0;
    ImportedVar *imp_vars = malloc(imp_var_cap * sizeof(ImportedVar));

    /* Process imports recursively */
    if (source_file) {
        import_push_file(source_file);
        process_imports(ast, source_file, &fn_table, &class_table, &enum_table,
                        &imp_vars, &imp_var_count, &imp_var_cap);
        import_pop_file();
    }

    for (ASTNode *n = ast; n; n = n->next) {
        if (n->type == NODE_FN_DECL) {
            if (fn_table_find(&fn_table, n->fn_name))
                diag_emit(n->loc, DIAG_ERROR, "duplicate function '%s'", n->fn_name);
            fn_table_add(&fn_table, n->fn_name, n);
        }
        if (n->type == NODE_CLASS_DECL) {
            if (class_table_find(&class_table, n->class_name))
                diag_emit(n->loc, DIAG_ERROR, "duplicate class '%s'", n->class_name);

            /* Flatten parent + own fields for inheritance */
            int total_fields = 0;
            char **all_names = NULL;
            ValueType *all_types = NULL;

            if (n->parent_class_name) {
                ClassDef *parent = class_table_find(&class_table, n->parent_class_name);
                if (!parent)
                    diag_emit(n->loc, DIAG_ERROR, "undefined parent class '%s'", n->parent_class_name);
                total_fields = parent->field_count + n->class_field_count;
                all_names = malloc(total_fields * sizeof(char *));
                all_types = malloc(total_fields * sizeof(ValueType));
                for (int i = 0; i < parent->field_count; i++) {
                    all_names[i] = parent->field_names[i];
                    all_types[i] = parent->field_types[i];
                }
                for (int i = 0; i < n->class_field_count; i++) {
                    all_names[parent->field_count + i] = n->class_fields[i].name;
                    all_types[parent->field_count + i] = n->class_fields[i].type;
                }
            } else {
                total_fields = n->class_field_count;
                all_names = malloc(total_fields * sizeof(char *));
                all_types = malloc(total_fields * sizeof(ValueType));
                for (int i = 0; i < n->class_field_count; i++) {
                    all_names[i] = n->class_fields[i].name;
                    all_types[i] = n->class_fields[i].type;
                }
            }

            if (class_table.count == class_table.cap) {
                class_table.cap *= 2;
                class_table.entries = realloc(class_table.entries, class_table.cap * sizeof(ClassDef));
            }
            ClassDef *cd = &class_table.entries[class_table.count++];
            cd->name = n->class_name;
            cd->parent_name = n->parent_class_name;
            cd->field_names = all_names;
            cd->field_types = all_types;
            cd->field_count = total_fields;
            cd->methods = n->class_methods;
            cd->loc = n->loc;
        }
        if (n->type == NODE_ENUM_DECL) {
            if (enum_table_find(&enum_table, n->enum_name))
                diag_emit(n->loc, DIAG_ERROR, "duplicate enum '%s'", n->enum_name);
            if (enum_table.count == enum_table.cap) {
                enum_table.cap *= 2;
                enum_table.entries = realloc(enum_table.entries, enum_table.cap * sizeof(EnumDef));
            }
            EnumDef *ed = &enum_table.entries[enum_table.count++];
            ed->name = n->enum_name;
            ed->variant_count = n->enum_variant_count;
            ed->variant_names = malloc(ed->variant_count * sizeof(char *));
            ed->variant_values = malloc(ed->variant_count * sizeof(long));
            for (int i = 0; i < ed->variant_count; i++) {
                ed->variant_names[i] = n->enum_variants[i].name;
                ed->variant_values[i] = n->enum_variants[i].value;
            }
            ed->loc = n->loc;
        }
    }

    SymTable st;
    sym_table_init(&st);

    /* Add imported variables to the main symbol table */
    for (int i = 0; i < imp_var_count; i++) {
        sym_add(&st, imp_vars[i].name, imp_vars[i].val, imp_vars[i].is_const, (SourceLoc){0, 0});
        /* Mark as mutated to suppress "never mutated" warning for imports */
        st.syms[st.count - 1].mutated = 1;
    }

    PrintList prints;
    print_list_init(&prints);

    /* Initialize IR program for potential runtime code */
    IRProgram ir_prog;
    ir_init(&ir_prog);
    g_ir = &ir_prog;
    g_ir_mode = 0;

    /* Set globals for EXPR_FN_CALL evaluation */
    g_ft = &fn_table;
    g_ct = &class_table;
    g_et = &enum_table;
    g_prints = &prints;

    eval_stmts(ast, &st, &fn_table, &class_table, &prints, NULL);

    for (int j = 0; j < st.count; j++) {
        if (!st.syms[j].is_const && !st.syms[j].mutated)
            diag_emit(st.syms[j].loc, DIAG_WARNING, "variable '%s' is never mutated, consider using 'const'", st.syms[j].name);
    }

    sym_table_free(&st);
    fn_table_free(&fn_table);
    class_table_free(&class_table);
    enum_table_free(&enum_table);

    int result;

    if (g_net_start_called) {
        /* Net mode — emit networking binary */
        result = emit_net_binary(&g_net_config, output_path);
        print_list_free(&prints);
    } else if (g_http_listen_called) {
        /* HTTP server mode — emit server binary instead of normal print binary */
        result = emit_http_binary(g_http_routes, g_http_route_count,
                                  g_http_listen_port, output_path);
        free(g_http_routes);
        print_list_free(&prints);
    } else if (g_ir_mode) {
        /* IR mode — emit IR-based binary with runtime code */
        ir_emit_exit(&ir_prog);
        print_list_free(&prints);
        result = emit_binary_ir(&ir_prog, output_path);
    } else {
        /* Normal mode — emit print binary (all compile-time) */
        int string_count = prints.count;
        int *str_offsets = malloc(string_count * sizeof(int));
        int *str_lengths_arr = malloc(string_count * sizeof(int));

        Buffer strings;
        buf_init(&strings);

        for (int i = 0; i < string_count; i++) {
            str_offsets[i] = strings.len;
            str_lengths_arr[i] = prints.lengths[i];
            buf_write(&strings, prints.strings[i], prints.lengths[i]);
        }

        print_list_free(&prints);

        result = emit_binary(string_count, str_offsets, str_lengths_arr,
                             &strings, output_path);

        free(str_offsets);
        free(str_lengths_arr);
        buf_free(&strings);
    }

    ir_free(&ir_prog);
    g_ir = NULL;

    /* Clean up imported variables */
    for (int i = 0; i < imp_var_count; i++)
        free(imp_vars[i].name);
    free(imp_vars);

    /* Clean up import module cache (must be after codegen since fn_table
       entries may point into cached ASTs) */
    if (source_file)
        import_cleanup();

    return result;
}
