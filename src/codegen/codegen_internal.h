#ifndef CODEGEN_INTERNAL_H
#define CODEGEN_INTERNAL_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <sys/stat.h>

/* ================================================================
 * Buffer — shared byte-buffer used by all backends
 * ================================================================ */

typedef struct {
    uint8_t *data;
    int len;
    int cap;
} Buffer;

static inline void buf_init(Buffer *b) {
    b->cap = 16384;
    b->data = calloc(1, b->cap);
    b->len = 0;
}

static inline void buf_ensure(Buffer *b, int need) {
    while (b->len + need > b->cap) {
        b->cap *= 2;
        b->data = realloc(b->data, b->cap);
        memset(b->data + b->len, 0, b->cap - b->len);
    }
}

static inline void buf_write(Buffer *b, const void *data, int len) {
    buf_ensure(b, len);
    memcpy(b->data + b->len, data, len);
    b->len += len;
}

static inline void buf_write8(Buffer *b, uint8_t val) {
    buf_write(b, &val, 1);
}

static inline void buf_write16(Buffer *b, uint16_t val) {
    buf_write(b, &val, 2);
}

static inline void buf_write32(Buffer *b, uint32_t val) {
    buf_write(b, &val, 4);
}

static inline void buf_write64(Buffer *b, uint64_t val) {
    buf_write(b, &val, 8);
}

static inline void buf_pad_to(Buffer *b, int target) {
    if (target <= b->len) return;
    buf_ensure(b, target - b->len);
    memset(b->data + b->len, 0, target - b->len);
    b->len = target;
}

static inline void buf_free(Buffer *b) {
    free(b->data);
    b->data = NULL;
    b->len = b->cap = 0;
}

/* ================================================================
 * emit_binary — implemented per target in its own .c file
 * ================================================================ */

int emit_binary(int string_count, int *str_offsets, int *str_lengths_arr,
                Buffer *strings, const char *output_path);

#endif
