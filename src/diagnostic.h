#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

typedef struct {
    int line;   /* 1-based */
    int col;    /* 1-based */
} SourceLoc;

#define LOC_NONE ((SourceLoc){0, 0})

typedef enum { DIAG_ERROR, DIAG_WARNING } DiagSeverity;

void diag_init(const char *filename, const char *source);
void diag_emit(SourceLoc loc, DiagSeverity severity, const char *fmt, ...)
    __attribute__((format(printf, 3, 4)));
void diag_error_no_loc(const char *fmt, ...)
    __attribute__((format(printf, 1, 2)));

typedef struct {
    const char *filename;
    const char *source;
} DiagContext;

DiagContext diag_save(void);
void diag_restore(DiagContext ctx);

#endif
