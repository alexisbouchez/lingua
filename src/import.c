#define _GNU_SOURCE
#include "import.h"
#include "lexer.h"
#include "diagnostic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <libgen.h>

/* ================================================================
 * Module cache — stores parsed ASTs so each file is only parsed once
 * ================================================================ */

typedef struct {
    char *abs_path;
    ASTNode *ast;
    char *source;
    char *filename;
} CachedModule;

static CachedModule *module_cache;
static int module_cache_count;
static int module_cache_cap;

/* ================================================================
 * Import stack — tracks files currently being processed for
 * circular import detection
 * ================================================================ */

static char **import_stack;
static int import_stack_count;
static int import_stack_cap;

/* Project root directory */
static char *project_root_dir;

/* ================================================================
 * Init / Cleanup
 * ================================================================ */

void import_init(const char *project_root) {
    project_root_dir = strdup(project_root);

    module_cache_cap = 8;
    module_cache_count = 0;
    module_cache = malloc(module_cache_cap * sizeof(CachedModule));

    import_stack_cap = 8;
    import_stack_count = 0;
    import_stack = malloc(import_stack_cap * sizeof(char *));
}

void import_cleanup(void) {
    for (int i = 0; i < module_cache_count; i++) {
        free(module_cache[i].abs_path);
        ast_free(module_cache[i].ast);
        free(module_cache[i].source);
        free(module_cache[i].filename);
    }
    free(module_cache);
    module_cache = NULL;
    module_cache_count = 0;
    module_cache_cap = 0;

    free(import_stack);
    import_stack = NULL;
    import_stack_count = 0;
    import_stack_cap = 0;

    free(project_root_dir);
    project_root_dir = NULL;
}

/* ================================================================
 * File reading (same logic as main.c)
 * ================================================================ */

static char *read_file(const char *path) {
    FILE *f = fopen(path, "r");
    if (!f)
        return NULL;
    fseek(f, 0, SEEK_END);
    long len = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = malloc(len + 1);
    fread(buf, 1, len, f);
    buf[len] = '\0';
    fclose(f);
    return buf;
}

/* ================================================================
 * Path resolution
 * ================================================================ */

static char *resolve_path(const char *importing_file, const char *import_path, SourceLoc loc) {
    char raw_path[PATH_MAX];

    if (import_path[0] == '.' && import_path[1] == '/') {
        /* Relative to the importing file's directory */
        char *importing_copy = strdup(importing_file);
        char *dir = dirname(importing_copy);
        snprintf(raw_path, sizeof(raw_path), "%s/%s.lingua", dir, import_path + 2);
        free(importing_copy);
    } else {
        /* Relative to project root */
        snprintf(raw_path, sizeof(raw_path), "%s/%s.lingua", project_root_dir, import_path);
    }

    char *resolved = realpath(raw_path, NULL);
    if (!resolved) {
        diag_emit(loc, DIAG_ERROR, "cannot find module '%s' (tried '%s')", import_path, raw_path);
    }
    return resolved;
}

/* ================================================================
 * Cache lookup
 * ================================================================ */

static CachedModule *cache_find(const char *abs_path) {
    for (int i = 0; i < module_cache_count; i++) {
        if (strcmp(module_cache[i].abs_path, abs_path) == 0)
            return &module_cache[i];
    }
    return NULL;
}

/* ================================================================
 * Circular import detection
 * ================================================================ */

static int is_in_import_stack(const char *abs_path) {
    for (int i = 0; i < import_stack_count; i++) {
        if (strcmp(import_stack[i], abs_path) == 0)
            return 1;
    }
    return 0;
}

static void push_import_stack(const char *abs_path) {
    if (import_stack_count == import_stack_cap) {
        import_stack_cap *= 2;
        import_stack = realloc(import_stack, import_stack_cap * sizeof(char *));
    }
    import_stack[import_stack_count++] = (char *)abs_path;
}

static void pop_import_stack(void) {
    if (import_stack_count > 0)
        import_stack_count--;
}

void import_push_file(const char *abs_path) {
    push_import_stack(abs_path);
}

void import_pop_file(void) {
    pop_import_stack();
}

/* ================================================================
 * import_resolve
 * ================================================================ */

int import_resolve(const char *importing_file, const char *import_path,
                   SourceLoc loc, ASTNode **out_ast,
                   const char **out_source, const char **out_filename) {
    char *abs_path = resolve_path(importing_file, import_path, loc);

    /* Check circular import */
    if (is_in_import_stack(abs_path)) {
        /* Build chain string for error message */
        char chain[2048];
        int pos = 0;
        for (int i = 0; i < import_stack_count; i++) {
            if (pos > 0) pos += snprintf(chain + pos, sizeof(chain) - pos, " -> ");
            pos += snprintf(chain + pos, sizeof(chain) - pos, "%s", import_stack[i]);
        }
        pos += snprintf(chain + pos, sizeof(chain) - pos, " -> %s", abs_path);
        free(abs_path);
        diag_emit(loc, DIAG_ERROR, "circular import detected: %s", chain);
        return 1; /* unreachable, diag_emit exits */
    }

    /* Check cache */
    CachedModule *cached = cache_find(abs_path);
    if (cached) {
        *out_ast = cached->ast;
        *out_source = cached->source;
        *out_filename = cached->filename;
        free(abs_path);
        return 0;
    }

    /* Read file */
    char *source = read_file(abs_path);
    if (!source) {
        diag_emit(loc, DIAG_ERROR, "cannot open module '%s'", abs_path);
        free(abs_path);
        return 1; /* unreachable */
    }

    /* Save diagnostic context and switch to the imported file */
    DiagContext saved = diag_save();
    diag_init(abs_path, source);

    /* Lex and parse */
    Lexer lexer;
    lexer_init(&lexer, source);
    ASTNode *ast = parse(&lexer);

    /* Restore diagnostic context */
    diag_restore(saved);

    /* Cache the result */
    if (module_cache_count == module_cache_cap) {
        module_cache_cap *= 2;
        module_cache = realloc(module_cache, module_cache_cap * sizeof(CachedModule));
    }
    CachedModule *mod = &module_cache[module_cache_count++];
    mod->abs_path = abs_path;
    mod->ast = ast;
    mod->source = source;
    mod->filename = strdup(abs_path);

    *out_ast = ast;
    *out_source = source;
    *out_filename = mod->filename;
    return 0;
}
