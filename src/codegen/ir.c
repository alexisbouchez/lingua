#include "codegen/ir.h"
#include <stdlib.h>
#include <string.h>

void ir_init(IRProgram *prog) {
    prog->instr_cap = 256;
    prog->instr_count = 0;
    prog->instrs = malloc(prog->instr_cap * sizeof(IRInstr));

    prog->string_cap = 32;
    prog->string_count = 0;
    prog->strings = malloc(prog->string_cap * sizeof(IRString));

    prog->next_vreg = 0;
    prog->next_label = 0;
    prog->next_slot = 0;
}

void ir_free(IRProgram *prog) {
    free(prog->instrs);
    free(prog->strings);
    prog->instrs = NULL;
    prog->strings = NULL;
    prog->instr_count = prog->instr_cap = 0;
    prog->string_count = prog->string_cap = 0;
}

int ir_alloc_vreg(IRProgram *prog) {
    return prog->next_vreg++;
}

int ir_alloc_label(IRProgram *prog) {
    return prog->next_label++;
}

int ir_alloc_slot(IRProgram *prog) {
    return prog->next_slot++;
}

int ir_add_string(IRProgram *prog, const char *data, int len) {
    if (prog->string_count == prog->string_cap) {
        prog->string_cap *= 2;
        prog->strings = realloc(prog->strings, prog->string_cap * sizeof(IRString));
    }
    int idx = prog->string_count++;
    prog->strings[idx].data = data;
    prog->strings[idx].len = len;
    return idx;
}

int ir_emit(IRProgram *prog, IRInstr instr) {
    if (prog->instr_count == prog->instr_cap) {
        prog->instr_cap *= 2;
        prog->instrs = realloc(prog->instrs, prog->instr_cap * sizeof(IRInstr));
    }
    int idx = prog->instr_count++;
    prog->instrs[idx] = instr;
    return idx;
}

int ir_emit_const_int(IRProgram *prog, int64_t value) {
    int dst = ir_alloc_vreg(prog);
    IRInstr instr;
    memset(&instr, 0, sizeof(instr));
    instr.op = IR_CONST_INT;
    instr.dst = dst;
    instr.imm = value;
    ir_emit(prog, instr);
    return dst;
}

int ir_emit_const_str(IRProgram *prog, const char *data, int len) {
    int dst = ir_alloc_vreg(prog);
    int idx = ir_add_string(prog, data, len);
    IRInstr instr;
    memset(&instr, 0, sizeof(instr));
    instr.op = IR_CONST_STR;
    instr.dst = dst;
    instr.str_idx = idx;
    instr.str_len = len;
    ir_emit(prog, instr);
    return dst;
}

int ir_emit_binop(IRProgram *prog, IROpcode op, int lhs, int rhs) {
    int dst = ir_alloc_vreg(prog);
    IRInstr instr;
    memset(&instr, 0, sizeof(instr));
    instr.op = op;
    instr.dst = dst;
    instr.lhs = lhs;
    instr.rhs = rhs;
    ir_emit(prog, instr);
    return dst;
}

int ir_emit_load(IRProgram *prog, int slot) {
    int dst = ir_alloc_vreg(prog);
    IRInstr instr;
    memset(&instr, 0, sizeof(instr));
    instr.op = IR_LOAD_LOCAL;
    instr.dst = dst;
    instr.slot = slot;
    ir_emit(prog, instr);
    return dst;
}

void ir_emit_store(IRProgram *prog, int slot, int src) {
    IRInstr instr;
    memset(&instr, 0, sizeof(instr));
    instr.op = IR_STORE_LOCAL;
    instr.dst = -1;
    instr.slot = slot;
    instr.src = src;
    ir_emit(prog, instr);
}

void ir_emit_print_int(IRProgram *prog, int src) {
    IRInstr instr;
    memset(&instr, 0, sizeof(instr));
    instr.op = IR_PRINT_INT;
    instr.dst = -1;
    instr.src = src;
    ir_emit(prog, instr);
}

void ir_emit_print_str(IRProgram *prog, const char *data, int len) {
    int idx = ir_add_string(prog, data, len);
    IRInstr instr;
    memset(&instr, 0, sizeof(instr));
    instr.op = IR_PRINT_STR;
    instr.dst = -1;
    instr.str_idx = idx;
    instr.str_len = len;
    ir_emit(prog, instr);
}

void ir_emit_print_bool(IRProgram *prog, int src) {
    IRInstr instr;
    memset(&instr, 0, sizeof(instr));
    instr.op = IR_PRINT_BOOL;
    instr.dst = -1;
    instr.src = src;
    ir_emit(prog, instr);
}

void ir_emit_label(IRProgram *prog, int label_id) {
    IRInstr instr;
    memset(&instr, 0, sizeof(instr));
    instr.op = IR_LABEL;
    instr.dst = -1;
    instr.label_id = label_id;
    ir_emit(prog, instr);
}

void ir_emit_jmp(IRProgram *prog, int label_id) {
    IRInstr instr;
    memset(&instr, 0, sizeof(instr));
    instr.op = IR_JMP;
    instr.dst = -1;
    instr.label_id = label_id;
    ir_emit(prog, instr);
}

void ir_emit_jz(IRProgram *prog, int src, int label_id) {
    IRInstr instr;
    memset(&instr, 0, sizeof(instr));
    instr.op = IR_JZ;
    instr.dst = -1;
    instr.src = src;
    instr.label_id = label_id;
    ir_emit(prog, instr);
}

void ir_emit_jnz(IRProgram *prog, int src, int label_id) {
    IRInstr instr;
    memset(&instr, 0, sizeof(instr));
    instr.op = IR_JNZ;
    instr.dst = -1;
    instr.src = src;
    instr.label_id = label_id;
    ir_emit(prog, instr);
}

void ir_emit_exit(IRProgram *prog) {
    IRInstr instr;
    memset(&instr, 0, sizeof(instr));
    instr.op = IR_EXIT;
    instr.dst = -1;
    ir_emit(prog, instr);
}
