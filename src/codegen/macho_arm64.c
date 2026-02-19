#if defined(__APPLE__) && defined(__aarch64__)

#include "codegen_internal.h"

/* ARM64 instruction encoding helpers */
static uint32_t movz(int rd, uint16_t imm, int shift) {
    return 0xD2800000 | ((shift / 16) << 21) | ((uint32_t)imm << 5) | rd;
}

static uint32_t adr(int rd, int32_t offset) {
    uint32_t immlo = (offset & 0x3) << 29;
    uint32_t immhi = ((offset >> 2) & 0x7FFFF) << 5;
    return 0x10000000 | immlo | immhi | rd;
}

static uint32_t svc(uint16_t imm) {
    return 0xD4000001 | ((uint32_t)imm << 5);
}

static void buf_write_segname(Buffer *b, const char *name) {
    char seg[16];
    memset(seg, 0, 16);
    strncpy(seg, name, 16);
    buf_write(b, seg, 16);
}

/* Mach-O constants */
#define MH_MAGIC_64        0xFEEDFACF
#define MH_EXECUTE         2
#define CPU_TYPE_ARM64     ((uint32_t)0x0100000C)
#define CPU_SUBTYPE_ALL    0
#define MH_NOUNDEFS        0x1
#define MH_DYLDLINK        0x4
#define MH_PIE             0x200000
#define MH_TWOLEVEL        0x80

#define LC_SEGMENT_64      0x19
#define LC_SYMTAB          0x02
#define LC_DYSYMTAB        0x0B
#define LC_LOAD_DYLIB      0x0C
#define LC_LOAD_DYLINKER   0x0E
#define LC_MAIN            ((uint32_t)(0x28 | 0x80000000))
#define LC_BUILD_VERSION   0x32
#define LC_DYLD_CHAINED_FIXUPS   ((uint32_t)(0x34 | 0x80000000))
#define LC_DYLD_EXPORTS_TRIE     ((uint32_t)(0x33 | 0x80000000))

#define VM_PROT_NONE       0
#define VM_PROT_READ       1
#define VM_PROT_EXECUTE    4

#define PLATFORM_MACOS     1
#define MACHO_PAGE_SIZE    16384

/*
 * Per print statement (20 bytes):
 *   adr  x1, <string>     (4 bytes)
 *   movz x0, #1           (4 bytes)  — stdout
 *   movz x2, #len         (4 bytes)
 *   movz x16, #4          (4 bytes)  — write syscall
 *   svc  #0x80            (4 bytes)
 *
 * Exit (12 bytes):
 *   movz x0, #0           (4 bytes)
 *   movz x16, #1          (4 bytes)  — exit syscall
 *   svc  #0x80            (4 bytes)
 */
#define PRINT_INSTR_SIZE  20
#define EXIT_INSTR_SIZE   12

int emit_binary(int string_count, int *str_offsets, int *str_lengths_arr,
                Buffer *strings, const char *output_path)
{
    int instr_size = string_count * PRINT_INSTR_SIZE + EXIT_INSTR_SIZE;

    Buffer code;
    buf_init(&code);

    for (int i = 0; i < string_count; i++) {
        int str_off = instr_size - code.len + str_offsets[i];
        buf_write32(&code, adr(1, str_off));
        buf_write32(&code, movz(0, 1, 0));
        buf_write32(&code, movz(2, str_lengths_arr[i], 0));
        buf_write32(&code, movz(16, 4, 0));
        buf_write32(&code, svc(0x80));
    }

    buf_write32(&code, movz(0, 0, 0));
    buf_write32(&code, movz(16, 1, 0));
    buf_write32(&code, svc(0x80));

    buf_write(&code, strings->data, strings->len);

    uint64_t text_vmaddr = 0x100000000ULL;

    int sz_seg_nosect = 72;
    int sz_seg_1sect  = 72 + 80;
    int sz_dylinker   = 32;
    int sz_main       = 24;
    int sz_build_ver  = 24;
    int sz_load_dylib = 56;
    int sz_chained    = 16;
    int sz_exports    = 16;
    int sz_symtab     = 24;
    int sz_dysymtab   = 80;

    int ncmds = 11;
    int sizeofcmds = sz_seg_nosect + sz_seg_1sect + sz_seg_nosect
                   + sz_dylinker + sz_main + sz_build_ver + sz_load_dylib
                   + sz_chained + sz_exports + sz_symtab + sz_dysymtab;

    int header_and_cmds = 32 + sizeofcmds;
    int code_offset = (header_and_cmds + 32 + 3) & ~3;

    uint64_t text_segment_vmsize = MACHO_PAGE_SIZE;
    uint64_t text_segment_filesize = MACHO_PAGE_SIZE;
    uint64_t linkedit_file_offset = MACHO_PAGE_SIZE;
    uint64_t linkedit_vmaddr = text_vmaddr + text_segment_vmsize;

    int chained_fixups_off = (int)linkedit_file_offset;
    int chained_fixups_size = 48;
    int exports_trie_off = chained_fixups_off + chained_fixups_size;
    int exports_trie_size = 8;
    int linkedit_total = chained_fixups_size + exports_trie_size;

    Buffer out;
    buf_init(&out);

    /* Mach-O Header (32 bytes) */
    buf_write32(&out, MH_MAGIC_64);
    buf_write32(&out, CPU_TYPE_ARM64);
    buf_write32(&out, CPU_SUBTYPE_ALL);
    buf_write32(&out, MH_EXECUTE);
    buf_write32(&out, ncmds);
    buf_write32(&out, sizeofcmds);
    buf_write32(&out, MH_NOUNDEFS | MH_DYLDLINK | MH_TWOLEVEL | MH_PIE);
    buf_write32(&out, 0);

    /* __PAGEZERO */
    buf_write32(&out, LC_SEGMENT_64);
    buf_write32(&out, sz_seg_nosect);
    buf_write_segname(&out, "__PAGEZERO");
    buf_write64(&out, 0);
    buf_write64(&out, 0x100000000ULL);
    buf_write64(&out, 0);
    buf_write64(&out, 0);
    buf_write32(&out, VM_PROT_NONE);
    buf_write32(&out, VM_PROT_NONE);
    buf_write32(&out, 0);
    buf_write32(&out, 0);

    /* __TEXT */
    buf_write32(&out, LC_SEGMENT_64);
    buf_write32(&out, sz_seg_1sect);
    buf_write_segname(&out, "__TEXT");
    buf_write64(&out, text_vmaddr);
    buf_write64(&out, text_segment_vmsize);
    buf_write64(&out, 0);
    buf_write64(&out, text_segment_filesize);
    buf_write32(&out, VM_PROT_READ | VM_PROT_EXECUTE);
    buf_write32(&out, VM_PROT_READ | VM_PROT_EXECUTE);
    buf_write32(&out, 1);
    buf_write32(&out, 0);

    /* Section: __text */
    buf_write_segname(&out, "__text");
    buf_write_segname(&out, "__TEXT");
    buf_write64(&out, text_vmaddr + code_offset);
    buf_write64(&out, code.len);
    buf_write32(&out, code_offset);
    buf_write32(&out, 2);
    buf_write32(&out, 0);
    buf_write32(&out, 0);
    buf_write32(&out, 0x80000400);
    buf_write32(&out, 0);
    buf_write32(&out, 0);
    buf_write32(&out, 0);

    /* __LINKEDIT */
    buf_write32(&out, LC_SEGMENT_64);
    buf_write32(&out, sz_seg_nosect);
    buf_write_segname(&out, "__LINKEDIT");
    buf_write64(&out, linkedit_vmaddr);
    buf_write64(&out, MACHO_PAGE_SIZE);
    buf_write64(&out, linkedit_file_offset);
    buf_write64(&out, linkedit_total);
    buf_write32(&out, VM_PROT_READ);
    buf_write32(&out, VM_PROT_READ);
    buf_write32(&out, 0);
    buf_write32(&out, 0);

    /* LC_LOAD_DYLINKER */
    buf_write32(&out, LC_LOAD_DYLINKER);
    buf_write32(&out, sz_dylinker);
    buf_write32(&out, 12);
    {
        char path[20];
        memset(path, 0, 20);
        strcpy(path, "/usr/lib/dyld");
        buf_write(&out, path, 20);
    }

    /* LC_MAIN */
    buf_write32(&out, LC_MAIN);
    buf_write32(&out, sz_main);
    buf_write64(&out, code_offset);
    buf_write64(&out, 0);

    /* LC_BUILD_VERSION */
    buf_write32(&out, LC_BUILD_VERSION);
    buf_write32(&out, sz_build_ver);
    buf_write32(&out, PLATFORM_MACOS);
    buf_write32(&out, 0x000E0000);
    buf_write32(&out, 0);
    buf_write32(&out, 0);

    /* LC_LOAD_DYLIB (libSystem) */
    buf_write32(&out, LC_LOAD_DYLIB);
    buf_write32(&out, sz_load_dylib);
    buf_write32(&out, 24);
    buf_write32(&out, 2);
    buf_write32(&out, 0x05540000);
    buf_write32(&out, 0x00010000);
    {
        char name[32];
        memset(name, 0, 32);
        strcpy(name, "/usr/lib/libSystem.B.dylib");
        buf_write(&out, name, 32);
    }

    /* LC_DYLD_CHAINED_FIXUPS */
    buf_write32(&out, LC_DYLD_CHAINED_FIXUPS);
    buf_write32(&out, sz_chained);
    buf_write32(&out, chained_fixups_off);
    buf_write32(&out, chained_fixups_size);

    /* LC_DYLD_EXPORTS_TRIE */
    buf_write32(&out, LC_DYLD_EXPORTS_TRIE);
    buf_write32(&out, sz_exports);
    buf_write32(&out, exports_trie_off);
    buf_write32(&out, exports_trie_size);

    /* LC_SYMTAB */
    buf_write32(&out, LC_SYMTAB);
    buf_write32(&out, sz_symtab);
    buf_write32(&out, 0);
    buf_write32(&out, 0);
    buf_write32(&out, 0);
    buf_write32(&out, 0);

    /* LC_DYSYMTAB */
    buf_write32(&out, LC_DYSYMTAB);
    buf_write32(&out, sz_dysymtab);
    for (int j = 0; j < 18; j++)
        buf_write32(&out, 0);

    buf_pad_to(&out, code_offset);
    buf_write(&out, code.data, code.len);
    buf_pad_to(&out, MACHO_PAGE_SIZE);

    /* __LINKEDIT: chained fixups */
    buf_write32(&out, 0);
    buf_write32(&out, 32);
    buf_write32(&out, 48);
    buf_write32(&out, 48);
    buf_write32(&out, 0);
    buf_write32(&out, 1);
    buf_write32(&out, 0);
    buf_write32(&out, 0);

    buf_write32(&out, 3);
    buf_write32(&out, 0);
    buf_write32(&out, 0);
    buf_write32(&out, 0);

    /* Exports trie (empty root) */
    {
        uint8_t trie[8] = {0};
        buf_write(&out, trie, 8);
    }

    /* Write to file */
    FILE *f = fopen(output_path, "wb");
    if (!f) {
        fprintf(stderr, "error: cannot open '%s' for writing\n", output_path);
        buf_free(&code); buf_free(&out);
        return 1;
    }

    fwrite(out.data, 1, out.len, f);
    fclose(f);
    chmod(output_path, 0755);

    /* Ad-hoc codesign (required on Apple Silicon) */
    char sign_cmd[2048];
    snprintf(sign_cmd, sizeof(sign_cmd),
             "codesign --force --sign - '%s' 2>/dev/null", output_path);
    system(sign_cmd);

    buf_free(&code);
    buf_free(&out);
    return 0;
}

#endif
