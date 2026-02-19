#if defined(__linux__) && defined(__x86_64__)

#include "codegen_internal.h"

#define ELF_HEADER_SIZE  64
#define PHDR_SIZE        56
#define ELF_CODE_OFFSET  (ELF_HEADER_SIZE + PHDR_SIZE)  /* 0x78 = 120 */
#define BASE_ADDR        0x400000ULL
#define ENTRY_ADDR       (BASE_ADDR + ELF_CODE_OFFSET)

/*
 * Per print statement (24 bytes):
 *   mov eax, 1              ; B8 01 00 00 00   (sys_write)
 *   mov edi, 1              ; BF 01 00 00 00   (stdout)
 *   lea rsi, [rip+disp32]   ; 48 8D 35 XX XX XX XX
 *   mov edx, <len>          ; BA XX XX XX XX
 *   syscall                 ; 0F 05
 *
 * Exit (9 bytes):
 *   mov eax, 60             ; B8 3C 00 00 00   (sys_exit)
 *   xor edi, edi            ; 31 FF             (status 0)
 *   syscall                 ; 0F 05
 */
#define PRINT_INSTR_SIZE 24
#define EXIT_INSTR_SIZE  9

int emit_binary(int string_count, int *str_offsets, int *str_lengths_arr,
                Buffer *strings, const char *output_path)
{
    int total_instr_size = string_count * PRINT_INSTR_SIZE + EXIT_INSTR_SIZE;

    Buffer code;
    buf_init(&code);

    for (int i = 0; i < string_count; i++) {
        int32_t rip_after_lea = i * PRINT_INSTR_SIZE + 17;
        int32_t target = total_instr_size + str_offsets[i];
        int32_t disp = target - rip_after_lea;

        /* mov eax, 1 */
        buf_write8(&code, 0xB8);
        buf_write32(&code, 1);
        /* mov edi, 1 */
        buf_write8(&code, 0xBF);
        buf_write32(&code, 1);
        /* lea rsi, [rip+disp32] */
        buf_write8(&code, 0x48);
        buf_write8(&code, 0x8D);
        buf_write8(&code, 0x35);
        buf_write32(&code, (uint32_t)disp);
        /* mov edx, <len> */
        buf_write8(&code, 0xBA);
        buf_write32(&code, (uint32_t)str_lengths_arr[i]);
        /* syscall */
        buf_write8(&code, 0x0F);
        buf_write8(&code, 0x05);
    }

    /* exit(0) */
    buf_write8(&code, 0xB8);
    buf_write32(&code, 60);
    buf_write8(&code, 0x31);
    buf_write8(&code, 0xFF);
    buf_write8(&code, 0x0F);
    buf_write8(&code, 0x05);

    buf_write(&code, strings->data, strings->len);

    uint64_t file_size = ELF_CODE_OFFSET + code.len;

    Buffer out;
    buf_init(&out);

    /* ELF64 Header (64 bytes) */
    buf_write8(&out, 0x7F);
    buf_write8(&out, 'E');
    buf_write8(&out, 'L');
    buf_write8(&out, 'F');
    buf_write8(&out, 2);              /* ELFCLASS64 */
    buf_write8(&out, 1);              /* ELFDATA2LSB */
    buf_write8(&out, 1);              /* EV_CURRENT */
    buf_write8(&out, 0);              /* ELFOSABI_NONE */
    buf_write64(&out, 0);             /* padding */
    buf_write16(&out, 2);             /* ET_EXEC */
    buf_write16(&out, 0x3E);          /* EM_X86_64 */
    buf_write32(&out, 1);             /* EV_CURRENT */
    buf_write64(&out, ENTRY_ADDR);    /* e_entry */
    buf_write64(&out, ELF_HEADER_SIZE); /* e_phoff */
    buf_write64(&out, 0);             /* e_shoff */
    buf_write32(&out, 0);             /* e_flags */
    buf_write16(&out, ELF_HEADER_SIZE); /* e_ehsize */
    buf_write16(&out, PHDR_SIZE);     /* e_phentsize */
    buf_write16(&out, 1);             /* e_phnum */
    buf_write16(&out, 0);             /* e_shentsize */
    buf_write16(&out, 0);             /* e_shnum */
    buf_write16(&out, 0);             /* e_shstrndx */

    /* Program Header: PT_LOAD (56 bytes) */
    buf_write32(&out, 1);             /* PT_LOAD */
    buf_write32(&out, 0x5);           /* PF_R | PF_X */
    buf_write64(&out, 0);             /* p_offset */
    buf_write64(&out, BASE_ADDR);     /* p_vaddr */
    buf_write64(&out, BASE_ADDR);     /* p_paddr */
    buf_write64(&out, file_size);     /* p_filesz */
    buf_write64(&out, file_size);     /* p_memsz */
    buf_write64(&out, 0x1000);        /* p_align */

    buf_write(&out, code.data, code.len);

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

    buf_free(&code);
    buf_free(&out);
    return 0;
}

#endif
