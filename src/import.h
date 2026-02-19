#ifndef IMPORT_H
#define IMPORT_H

#include "parser.h"
#include "diagnostic.h"

/* Initialize the import system with the project root directory */
void import_init(const char *project_root);

/* Resolve an import: parse the target file, return its AST.
   importing_file: absolute path of the file doing the import.
   import_path:    the path string from the import statement.
   loc:            source location for error messages.
   out_ast:        receives the parsed AST of the imported file.
   out_source:     receives the source text (owned by module cache).
   out_filename:   receives the filename (owned by module cache).
   Returns 0 on success, non-zero on failure. */
int import_resolve(const char *importing_file, const char *import_path,
                   SourceLoc loc, ASTNode **out_ast,
                   const char **out_source, const char **out_filename);

/* Push/pop a file path onto the import stack (for circular detection) */
void import_push_file(const char *abs_path);
void import_pop_file(void);

/* Free all cached modules and import state */
void import_cleanup(void);

#endif
