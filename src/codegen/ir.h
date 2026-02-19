#ifndef IR_H
#define IR_H

#include <stdint.h>

/* ================================================================
 * IR Instruction Set
 *
 * Virtual-register based IR. Each instruction uses unlimited vregs
 * (SSA-lite). Backends map vregs to stack slots.
 * ================================================================ */

typedef enum {
    /* Constants */
    IR_CONST_INT,       /* dst = imm64 */
    IR_CONST_STR,       /* dst = string_table[str_idx] (len = str_len) */

    /* Local variable access */
    IR_LOAD_LOCAL,      /* dst = stack[slot] */
    IR_STORE_LOCAL,     /* stack[slot] = src */

    /* Arithmetic (dst = lhs OP rhs) */
    IR_ADD,
    IR_SUB,
    IR_MUL,
    IR_DIV,
    IR_MOD,

    /* Unary */
    IR_NEG,             /* dst = -src */

    /* Bitwise (dst = lhs OP rhs) */
    IR_BIT_AND,
    IR_BIT_OR,
    IR_BIT_XOR,
    IR_BIT_NOT,         /* dst = ~src (unary) */
    IR_SHL,
    IR_SHR,

    /* Comparison (dst = lhs CMP rhs, result 0 or 1) */
    IR_CMP_EQ,
    IR_CMP_NE,
    IR_CMP_LT,
    IR_CMP_LE,
    IR_CMP_GT,
    IR_CMP_GE,

    /* Control flow */
    IR_LABEL,           /* label_id: */
    IR_JMP,             /* goto label_id */
    IR_JZ,              /* if src == 0 goto label_id */
    IR_JNZ,             /* if src != 0 goto label_id */

    /* Output */
    IR_PRINT_STR,       /* write(1, string[str_idx], str_len) */
    IR_PRINT_INT,       /* write(1, itoa(src), computed_len) */
    IR_PRINT_BOOL,      /* write(1, src ? "true" : "false", 4 or 5) */

    /* Terminator */
    IR_EXIT,            /* exit(0) */
} IROpcode;

typedef struct {
    IROpcode op;
    int dst;            /* destination vreg (-1 if unused) */
    int src;            /* source vreg (for unary / store / jumps) */
    int lhs;            /* left operand vreg (for binary ops) */
    int rhs;            /* right operand vreg (for binary ops) */
    int64_t imm;        /* immediate value (IR_CONST_INT) */
    int slot;           /* local variable slot (LOAD/STORE_LOCAL) */
    int label_id;       /* label identifier (LABEL/JMP/JZ/JNZ) */
    int str_idx;        /* string table index (CONST_STR/PRINT_STR) */
    int str_len;        /* string length (CONST_STR/PRINT_STR) */
} IRInstr;

/* String table entry for IR */
typedef struct {
    const char *data;
    int len;
} IRString;

typedef struct {
    IRInstr *instrs;
    int instr_count;
    int instr_cap;

    IRString *strings;
    int string_count;
    int string_cap;

    int next_vreg;      /* next virtual register number */
    int next_label;     /* next label number */
    int next_slot;      /* next local variable slot */
} IRProgram;

/* Initialize an IR program */
void ir_init(IRProgram *prog);

/* Free an IR program */
void ir_free(IRProgram *prog);

/* Allocate a new virtual register, returns vreg number */
int ir_alloc_vreg(IRProgram *prog);

/* Allocate a new label, returns label ID */
int ir_alloc_label(IRProgram *prog);

/* Allocate a new local variable slot, returns slot number */
int ir_alloc_slot(IRProgram *prog);

/* Add a string to the string table, returns string index */
int ir_add_string(IRProgram *prog, const char *data, int len);

/* Emit an instruction and return its index */
int ir_emit(IRProgram *prog, IRInstr instr);

/* Convenience: emit IR_CONST_INT */
int ir_emit_const_int(IRProgram *prog, int64_t value);

/* Convenience: emit IR_CONST_STR */
int ir_emit_const_str(IRProgram *prog, const char *data, int len);

/* Convenience: emit binary op (ADD, SUB, MUL, DIV, MOD, etc.) */
int ir_emit_binop(IRProgram *prog, IROpcode op, int lhs, int rhs);

/* Convenience: emit IR_LOAD_LOCAL */
int ir_emit_load(IRProgram *prog, int slot);

/* Convenience: emit IR_STORE_LOCAL */
void ir_emit_store(IRProgram *prog, int slot, int src);

/* Convenience: emit IR_PRINT_INT */
void ir_emit_print_int(IRProgram *prog, int src);

/* Convenience: emit IR_PRINT_STR */
void ir_emit_print_str(IRProgram *prog, const char *data, int len);

/* Convenience: emit IR_PRINT_BOOL */
void ir_emit_print_bool(IRProgram *prog, int src);

/* Convenience: emit IR_LABEL */
void ir_emit_label(IRProgram *prog, int label_id);

/* Convenience: emit IR_JMP */
void ir_emit_jmp(IRProgram *prog, int label_id);

/* Convenience: emit IR_JZ */
void ir_emit_jz(IRProgram *prog, int src, int label_id);

/* Convenience: emit IR_JNZ */
void ir_emit_jnz(IRProgram *prog, int src, int label_id);

/* Convenience: emit IR_EXIT */
void ir_emit_exit(IRProgram *prog);

#endif
