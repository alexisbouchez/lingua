#include "diagnostic.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const char *g_filename;
static const char *g_source;

void diag_init(const char *filename, const char *source) {
    g_filename = filename;
    g_source = source;
}

/* Find the start of a 1-based line number in source and return its length */
static const char *find_source_line(const char *source, int line, int *out_len) {
    const char *p = source;
    int cur = 1;
    while (*p && cur < line) {
        if (*p == '\n')
            cur++;
        p++;
    }
    const char *start = p;
    while (*p && *p != '\n')
        p++;
    *out_len = (int)(p - start);
    return start;
}

static void print_source_caret(SourceLoc loc) {
    if (!g_source || loc.line <= 0)
        return;

    int line_len;
    const char *line_start = find_source_line(g_source, loc.line, &line_len);

    /* line number width for alignment */
    int lineno_width = 0;
    int tmp = loc.line;
    while (tmp > 0) { lineno_width++; tmp /= 10; }

    fprintf(stderr, " %*d | %.*s\n", lineno_width, loc.line, line_len, line_start);
    fprintf(stderr, " %*s | ", lineno_width, "");
    for (int i = 1; i < loc.col; i++)
        fputc(' ', stderr);
    fprintf(stderr, "\033[1;32m^\033[0m\n");
}

void diag_emit(SourceLoc loc, DiagSeverity severity, const char *fmt, ...) {
    /* filename:line:col: */
    if (g_filename && loc.line > 0)
        fprintf(stderr, "\033[1m%s:%d:%d: \033[0m", g_filename, loc.line, loc.col);

    /* severity label */
    if (severity == DIAG_ERROR)
        fprintf(stderr, "\033[1;31merror:\033[0m ");
    else
        fprintf(stderr, "\033[1;33mwarning:\033[0m ");

    /* message */
    fprintf(stderr, "\033[1m");
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, "\033[0m\n");

    /* source caret */
    if (loc.line > 0)
        print_source_caret(loc);

    if (severity == DIAG_ERROR)
        exit(1);
}

DiagContext diag_save(void) {
    return (DiagContext){g_filename, g_source};
}

void diag_restore(DiagContext ctx) {
    g_filename = ctx.filename;
    g_source = ctx.source;
}

void diag_error_no_loc(const char *fmt, ...) {
    fprintf(stderr, "\033[1;31merror:\033[0m \033[1m");
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, "\033[0m\n");
    exit(1);
}
