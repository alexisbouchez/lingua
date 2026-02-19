#if defined(__linux__) && defined(__x86_64__)

#include "codegen_internal.h"
#include "diagnostic.h"

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
        diag_error_no_loc("cannot open '%s' for writing", output_path);
    }

    fwrite(out.data, 1, out.len, f);
    fclose(f);
    chmod(output_path, 0755);

    buf_free(&code);
    buf_free(&out);
    return 0;
}

/* ================================================================
 * emit_http_binary — emit an ELF binary containing an HTTP server
 *
 * The generated binary:
 *   1. socket(AF_INET, SOCK_STREAM, 0)
 *   2. setsockopt(SO_REUSEADDR)
 *   3. bind(sockaddr_in on given port)
 *   4. listen(backlog=128)
 *   5. accept loop:
 *      a. accept(server_fd, NULL, NULL)
 *      b. read(client_fd, stack_buffer, 4096)
 *      c. Parse HTTP method + path from first line
 *      d. Match against route table
 *      e. write(client_fd, response, len)
 *      f. close(client_fd)
 *      g. jmp accept_loop
 *
 * Data section layout (after code):
 *   - Pre-built HTTP response strings for each route
 *   - 404 response string
 *   - Route table: {method_len(u16), path_off(u16), path_len(u16),
 *                   response_off(u32), response_len(u32)} per route
 *   - sockaddr_in struct
 * ================================================================ */

/* Helper: emit mov reg32, imm32 */
static void emit_mov_r32_imm32(Buffer *c, uint8_t reg, uint32_t imm) {
    buf_write8(c, 0xB8 + reg);  /* B8+rd */
    buf_write32(c, imm);
}

/* Helper: emit syscall */
static void emit_syscall(Buffer *c) {
    buf_write8(c, 0x0F);
    buf_write8(c, 0x05);
}

/* Helper: patch a 32-bit relative displacement at a given offset */
static void patch_rel32(Buffer *c, int patch_offset, int target) {
    int32_t disp = target - (patch_offset + 4);
    memcpy(c->data + patch_offset, &disp, 4);
}

int emit_http_binary(HttpRouteEntry *routes, int route_count, int port,
                     const char *output_path)
{
    Buffer code;
    buf_init(&code);

    /*
     * Build data section first so we know offsets.
     * Data will be appended after code.
     */
    Buffer data;
    buf_init(&data);

    /* Pre-build HTTP response strings and record their offsets/lengths */
    int *resp_offsets = malloc((route_count + 1) * sizeof(int));
    int *resp_lengths = malloc((route_count + 1) * sizeof(int));

    for (int i = 0; i < route_count; i++) {
        resp_offsets[i] = data.len;
        /* Build: "HTTP/1.1 200 OK\r\nContent-Length: N\r\nConnection: close\r\n\r\nbody" */
        char header[256];
        int hlen = snprintf(header, sizeof(header),
                            "HTTP/1.1 200 OK\r\nContent-Length: %d\r\nConnection: close\r\n\r\n",
                            routes[i].body_len);
        buf_write(&data, header, hlen);
        buf_write(&data, routes[i].body, routes[i].body_len);
        resp_lengths[i] = hlen + routes[i].body_len;
    }

    /* 404 response */
    const char *not_found_body = "Not Found";
    int not_found_body_len = 9;
    char nf_header[256];
    int nf_hlen = snprintf(nf_header, sizeof(nf_header),
                           "HTTP/1.1 404 Not Found\r\nContent-Length: %d\r\nConnection: close\r\n\r\n",
                           not_found_body_len);
    resp_offsets[route_count] = data.len;
    buf_write(&data, nf_header, nf_hlen);
    buf_write(&data, not_found_body, not_found_body_len);
    resp_lengths[route_count] = nf_hlen + not_found_body_len;

    /* Route method strings (GET, POST) — store them in data section */
    int *method_offsets = malloc(route_count * sizeof(int));
    int *method_lengths = malloc(route_count * sizeof(int));
    for (int i = 0; i < route_count; i++) {
        method_offsets[i] = data.len;
        method_lengths[i] = (int)strlen(routes[i].method);
        buf_write(&data, routes[i].method, method_lengths[i]);
    }

    /* Route path strings */
    int *path_offsets = malloc(route_count * sizeof(int));
    for (int i = 0; i < route_count; i++) {
        path_offsets[i] = data.len;
        buf_write(&data, routes[i].path, routes[i].path_len);
    }

    /* sockaddr_in struct (16 bytes):
     *   sin_family: AF_INET (2) as u16
     *   sin_port: htons(port) as u16
     *   sin_addr: INADDR_ANY (0) as u32
     *   padding: 8 bytes of zeros
     */
    int sockaddr_offset = data.len;
    uint16_t sin_family = 2;  /* AF_INET */
    uint16_t sin_port = (uint16_t)(((port & 0xFF) << 8) | ((port >> 8) & 0xFF));  /* htons */
    buf_write16(&data, sin_family);
    buf_write16(&data, sin_port);
    buf_write32(&data, 0);  /* INADDR_ANY */
    buf_write64(&data, 0);  /* padding */

    /*
     * Now emit the machine code.
     * Register usage:
     *   r12 = server_fd (callee-saved)
     *   r13 = base address of data section (callee-saved)
     *   stack: 4096 bytes for read buffer
     *
     * All data references use r13 + offset.
     */

    /* === Prologue: set up stack frame with 4096-byte read buffer === */
    /* push rbp */
    buf_write8(&code, 0x55);
    /* mov rbp, rsp */
    buf_write8(&code, 0x48); buf_write8(&code, 0x89); buf_write8(&code, 0xE5);
    /* sub rsp, 4112 (4096 read buf + 16 alignment) */
    buf_write8(&code, 0x48); buf_write8(&code, 0x81); buf_write8(&code, 0xEC);
    buf_write32(&code, 4112);
    /* push r12 */
    buf_write8(&code, 0x41); buf_write8(&code, 0x54);
    /* push r13 */
    buf_write8(&code, 0x41); buf_write8(&code, 0x55);

    /* === Load data base address into r13 using lea r13, [rip+disp32] === */
    /* We'll patch this displacement later once we know the code size */
    buf_write8(&code, 0x4C); buf_write8(&code, 0x8D); buf_write8(&code, 0x2D);
    int data_base_patch = code.len;
    buf_write32(&code, 0); /* placeholder — patched later */

    /* === 1. socket(AF_INET=2, SOCK_STREAM=1, 0) === */
    /* syscall 41 */
    emit_mov_r32_imm32(&code, 0, 41);           /* mov eax, 41 */
    emit_mov_r32_imm32(&code, 7, 2);            /* mov edi, 2 (AF_INET) */
    emit_mov_r32_imm32(&code, 6, 1);            /* mov esi, 1 (SOCK_STREAM) */
    /* xor edx, edx */
    buf_write8(&code, 0x31); buf_write8(&code, 0xD2);
    emit_syscall(&code);
    /* mov r12, rax (save server_fd) */
    buf_write8(&code, 0x49); buf_write8(&code, 0x89); buf_write8(&code, 0xC4);

    /* === 2. setsockopt(fd, SOL_SOCKET=1, SO_REUSEADDR=2, &1, 4) === */
    /* Push 1 onto stack for the option value */
    /* mov dword [rsp], 1 */
    buf_write8(&code, 0xC7); buf_write8(&code, 0x04); buf_write8(&code, 0x24);
    buf_write32(&code, 1);
    /* syscall 54 */
    emit_mov_r32_imm32(&code, 0, 54);           /* mov eax, 54 */
    /* mov rdi, r12 */
    buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7);
    emit_mov_r32_imm32(&code, 6, 1);            /* mov esi, 1 (SOL_SOCKET) */
    emit_mov_r32_imm32(&code, 2, 2);            /* mov edx, 2 (SO_REUSEADDR) */
    /* mov r10, rsp */
    buf_write8(&code, 0x49); buf_write8(&code, 0x89); buf_write8(&code, 0xE2);
    /* mov r8d, 4 */
    buf_write8(&code, 0x41); buf_write8(&code, 0xB8); buf_write32(&code, 4);
    emit_syscall(&code);

    /* === 3. bind(fd, &sockaddr, 16) === */
    emit_mov_r32_imm32(&code, 0, 49);           /* mov eax, 49 (sys_bind) */
    /* mov rdi, r12 */
    buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7);
    /* lea rsi, [r13 + sockaddr_offset] */
    buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
    buf_write32(&code, (uint32_t)sockaddr_offset);
    emit_mov_r32_imm32(&code, 2, 16);           /* mov edx, 16 */
    emit_syscall(&code);

    /* Check bind result — if negative, exit with error */
    /* test rax, rax */
    buf_write8(&code, 0x48); buf_write8(&code, 0x85); buf_write8(&code, 0xC0);
    /* jns +skip (2-byte jns) */
    buf_write8(&code, 0x79);
    int bind_ok_patch = code.len;
    buf_write8(&code, 0x00); /* placeholder */
    /* Error: exit(1) */
    emit_mov_r32_imm32(&code, 0, 60);           /* mov eax, 60 (sys_exit) */
    emit_mov_r32_imm32(&code, 7, 1);            /* mov edi, 1 */
    emit_syscall(&code);
    /* patch jns target */
    code.data[bind_ok_patch] = (uint8_t)(code.len - bind_ok_patch - 1);

    /* === 4. listen(fd, 128) === */
    emit_mov_r32_imm32(&code, 0, 50);           /* mov eax, 50 (sys_listen) */
    /* mov rdi, r12 */
    buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7);
    emit_mov_r32_imm32(&code, 6, 128);          /* mov esi, 128 (backlog) */
    emit_syscall(&code);

    /* === accept_loop label === */
    int accept_loop = code.len;

    /* === 5. accept(fd, NULL, NULL) === */
    emit_mov_r32_imm32(&code, 0, 43);           /* mov eax, 43 (sys_accept) */
    /* mov rdi, r12 */
    buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7);
    /* xor esi, esi */
    buf_write8(&code, 0x31); buf_write8(&code, 0xF6);
    /* xor edx, edx */
    buf_write8(&code, 0x31); buf_write8(&code, 0xD2);
    emit_syscall(&code);

    /* Check accept result — if negative, loop back */
    /* test rax, rax */
    buf_write8(&code, 0x48); buf_write8(&code, 0x85); buf_write8(&code, 0xC0);
    /* js accept_loop (jmp back on error) */
    buf_write8(&code, 0x0F); buf_write8(&code, 0x88);
    int accept_err_patch = code.len;
    buf_write32(&code, 0); /* placeholder */

    /* Save client_fd in r14 */
    /* mov r14, rax */
    buf_write8(&code, 0x49); buf_write8(&code, 0x89); buf_write8(&code, 0xC6);

    /* === 6. read(client_fd, buf, 4096) === */
    emit_mov_r32_imm32(&code, 0, 0);            /* mov eax, 0 (sys_read) */
    /* mov rdi, r14 */
    buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xF7);
    /* lea rsi, [rbp - 4096] */
    buf_write8(&code, 0x48); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
    buf_write32(&code, (uint32_t)(-4096));
    emit_mov_r32_imm32(&code, 2, 4096);         /* mov edx, 4096 */
    emit_syscall(&code);

    /* Save bytes_read in r15 */
    /* mov r15, rax */
    buf_write8(&code, 0x49); buf_write8(&code, 0x89); buf_write8(&code, 0xC7);

    /* If read <= 0, close and loop back */
    /* test rax, rax */
    buf_write8(&code, 0x48); buf_write8(&code, 0x85); buf_write8(&code, 0xC0);
    /* jle close_client (forward jump, patch later) */
    buf_write8(&code, 0x0F); buf_write8(&code, 0x8E);
    int read_fail_patch = code.len;
    buf_write32(&code, 0); /* placeholder */

    /*
     * === 7. Parse HTTP request first line ===
     * Request buffer at [rbp - 4096]
     * Format: "METHOD /path HTTP/1.1\r\n..."
     *
     * We need to find:
     *   - method: from start to first space
     *   - path: from first space+1 to second space
     *
     * Use rcx as pointer into the buffer, rdx as end.
     */

    /* lea rcx, [rbp - 4096]   ; rcx = buf start */
    buf_write8(&code, 0x48); buf_write8(&code, 0x8D); buf_write8(&code, 0x8D);
    buf_write32(&code, (uint32_t)(-4096));
    /* lea rdx, [rcx + r15]    ; rdx = buf end (rcx + bytes_read) */
    buf_write8(&code, 0x4A); buf_write8(&code, 0x8D); buf_write8(&code, 0x14); buf_write8(&code, 0x39);

    /* Save buf_start in rsi for later method comparison */
    /* mov rsi, rcx  ; rsi = method_start */
    buf_write8(&code, 0x48); buf_write8(&code, 0x89); buf_write8(&code, 0xCE);

    /* Find first space — method_end */
    /* find_space1: */
    int find_space1 = code.len;
    /* cmp rcx, rdx */
    buf_write8(&code, 0x48); buf_write8(&code, 0x39); buf_write8(&code, 0xD1);
    /* jge send_404 (buffer ended without space) */
    buf_write8(&code, 0x0F); buf_write8(&code, 0x8D);
    int space1_fail_patch = code.len;
    buf_write32(&code, 0); /* placeholder — will jump to send_404 */
    /* cmp byte [rcx], ' ' */
    buf_write8(&code, 0x80); buf_write8(&code, 0x39); buf_write8(&code, 0x20);
    /* je found_space1 */
    buf_write8(&code, 0x74);
    int found_space1_patch = code.len;
    buf_write8(&code, 0x00); /* placeholder */
    /* inc rcx */
    buf_write8(&code, 0x48); buf_write8(&code, 0xFF); buf_write8(&code, 0xC1);
    /* jmp find_space1 */
    buf_write8(&code, 0xE9);
    int loop_back1_patch = code.len;
    buf_write32(&code, 0);
    patch_rel32(&code, loop_back1_patch, find_space1);

    /* found_space1: */
    int found_space1 = code.len;
    code.data[found_space1_patch] = (uint8_t)(found_space1 - found_space1_patch - 1);

    /* rcx now points at the first space. method is [rsi .. rcx) */
    /* Save method length: r8 = rcx - rsi (method_len) */
    /* mov r8, rcx */
    buf_write8(&code, 0x49); buf_write8(&code, 0x89); buf_write8(&code, 0xC8);
    /* sub r8, rsi */
    buf_write8(&code, 0x49); buf_write8(&code, 0x29); buf_write8(&code, 0xF0);

    /* inc rcx (skip space) — now rcx = path_start */
    buf_write8(&code, 0x48); buf_write8(&code, 0xFF); buf_write8(&code, 0xC1);
    /* mov r9, rcx  ; r9 = path_start */
    buf_write8(&code, 0x49); buf_write8(&code, 0x89); buf_write8(&code, 0xC9);

    /* Find second space — path_end */
    int find_space2 = code.len;
    /* cmp rcx, rdx */
    buf_write8(&code, 0x48); buf_write8(&code, 0x39); buf_write8(&code, 0xD1);
    /* jge send_404 */
    buf_write8(&code, 0x0F); buf_write8(&code, 0x8D);
    int space2_fail_patch = code.len;
    buf_write32(&code, 0); /* placeholder */
    /* cmp byte [rcx], ' ' */
    buf_write8(&code, 0x80); buf_write8(&code, 0x39); buf_write8(&code, 0x20);
    /* je found_space2 */
    buf_write8(&code, 0x74);
    int found_space2_patch = code.len;
    buf_write8(&code, 0x00);
    /* inc rcx */
    buf_write8(&code, 0x48); buf_write8(&code, 0xFF); buf_write8(&code, 0xC1);
    /* jmp find_space2 */
    buf_write8(&code, 0xE9);
    int loop_back2_patch = code.len;
    buf_write32(&code, 0);
    patch_rel32(&code, loop_back2_patch, find_space2);

    /* found_space2: */
    int found_space2 = code.len;
    code.data[found_space2_patch] = (uint8_t)(found_space2 - found_space2_patch - 1);

    /* rcx = path_end. path is [r9 .. rcx). */
    /* r10 = path_len = rcx - r9 */
    /* mov r10, rcx */
    buf_write8(&code, 0x49); buf_write8(&code, 0x89); buf_write8(&code, 0xCA);
    /* sub r10, r9 */
    buf_write8(&code, 0x4D); buf_write8(&code, 0x29); buf_write8(&code, 0xCA);

    /*
     * At this point:
     *   rsi  = method_start (in read buffer)
     *   r8   = method_len
     *   r9   = path_start (in read buffer)
     *   r10  = path_len
     *   r13  = data base address
     *
     * === 8. Route matching loop ===
     * For each route, compare method + path.
     */

    /* We'll generate unrolled comparisons for each route */
    int *route_match_patches = malloc(route_count * sizeof(int));

    for (int i = 0; i < route_count; i++) {
        /* Compare method length */
        /* cmp r8d, method_len */
        buf_write8(&code, 0x41); buf_write8(&code, 0x83); buf_write8(&code, 0xF8);
        buf_write8(&code, (uint8_t)method_lengths[i]);
        /* jne next_route */
        buf_write8(&code, 0x0F); buf_write8(&code, 0x85);
        int method_len_skip = code.len;
        buf_write32(&code, 0); /* placeholder */

        /* Compare method bytes using cmpsb-style loop:
         * Use rdi/rcx for method comparison */
        /* Save rsi (method_start) */
        /* push rsi */
        buf_write8(&code, 0x56);
        /* push r9, push r10 (save path regs) */
        buf_write8(&code, 0x41); buf_write8(&code, 0x51); /* push r9 */
        buf_write8(&code, 0x41); buf_write8(&code, 0x52); /* push r10 */

        /* lea rdi, [r13 + method_offset] ; data method string */
        buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0xBD);
        buf_write32(&code, (uint32_t)method_offsets[i]);
        /* mov ecx, method_len */
        emit_mov_r32_imm32(&code, 1, (uint32_t)method_lengths[i]);
        /* repe cmpsb */
        buf_write8(&code, 0xF3); buf_write8(&code, 0xA6);

        /* pop r10, pop r9, pop rsi (restore) */
        buf_write8(&code, 0x41); buf_write8(&code, 0x5A); /* pop r10 */
        buf_write8(&code, 0x41); buf_write8(&code, 0x59); /* pop r9 */
        buf_write8(&code, 0x5E); /* pop rsi */

        /* jne next_route */
        buf_write8(&code, 0x0F); buf_write8(&code, 0x85);
        int method_cmp_skip = code.len;
        buf_write32(&code, 0); /* placeholder */

        /* Compare path length */
        /* cmp r10d, path_len */
        buf_write8(&code, 0x41); buf_write8(&code, 0x83); buf_write8(&code, 0xFA);
        buf_write8(&code, (uint8_t)routes[i].path_len);
        /* jne next_route */
        buf_write8(&code, 0x0F); buf_write8(&code, 0x85);
        int path_len_skip = code.len;
        buf_write32(&code, 0); /* placeholder */

        /* Compare path bytes */
        /* push rsi, push r9, push r10 */
        buf_write8(&code, 0x56);
        buf_write8(&code, 0x41); buf_write8(&code, 0x51);
        buf_write8(&code, 0x41); buf_write8(&code, 0x52);

        /* mov rsi, r9 ; path in request */
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xCE);
        /* lea rdi, [r13 + path_offset] */
        buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0xBD);
        buf_write32(&code, (uint32_t)path_offsets[i]);
        /* mov ecx, path_len */
        emit_mov_r32_imm32(&code, 1, (uint32_t)routes[i].path_len);
        /* repe cmpsb */
        buf_write8(&code, 0xF3); buf_write8(&code, 0xA6);

        /* pop r10, pop r9, pop rsi */
        buf_write8(&code, 0x41); buf_write8(&code, 0x5A);
        buf_write8(&code, 0x41); buf_write8(&code, 0x59);
        buf_write8(&code, 0x5E);

        /* jne next_route */
        buf_write8(&code, 0x0F); buf_write8(&code, 0x85);
        int path_cmp_skip = code.len;
        buf_write32(&code, 0); /* placeholder */

        /* === Match found! Load response pointer and length, jump to send === */
        /* lea rsi, [r13 + resp_offset] */
        buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
        buf_write32(&code, (uint32_t)resp_offsets[i]);
        /* mov edx, resp_len */
        emit_mov_r32_imm32(&code, 2, (uint32_t)resp_lengths[i]);
        /* jmp send_response */
        buf_write8(&code, 0xE9);
        route_match_patches[i] = code.len;
        buf_write32(&code, 0); /* placeholder */

        /* next_route: — patch all the skip jumps to here */
        int next_route = code.len;
        patch_rel32(&code, method_len_skip, next_route);
        patch_rel32(&code, method_cmp_skip, next_route);
        patch_rel32(&code, path_len_skip, next_route);
        patch_rel32(&code, path_cmp_skip, next_route);
    }

    /* === 9. No route matched — send 404 === */
    int send_404 = code.len;

    /* Patch the space-not-found jumps to here */
    patch_rel32(&code, space1_fail_patch, send_404);
    patch_rel32(&code, space2_fail_patch, send_404);

    /* lea rsi, [r13 + 404_resp_offset] */
    buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
    buf_write32(&code, (uint32_t)resp_offsets[route_count]);
    /* mov edx, 404_resp_len */
    emit_mov_r32_imm32(&code, 2, (uint32_t)resp_lengths[route_count]);

    /* === send_response label === */
    int send_response = code.len;

    /* Patch all route match jumps to here */
    for (int i = 0; i < route_count; i++) {
        patch_rel32(&code, route_match_patches[i], send_response);
    }

    /* === 10. write(client_fd, rsi, rdx) === */
    /* rsi = response, edx = length (already set) */
    emit_mov_r32_imm32(&code, 0, 1);            /* mov eax, 1 (sys_write) */
    /* mov rdi, r14 (client_fd) */
    buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xF7);
    /* rsi and rdx already set */
    emit_syscall(&code);

    /* === 11. close(client_fd) === */
    int close_client = code.len;

    /* Patch read_fail jump to here */
    patch_rel32(&code, read_fail_patch, close_client);

    emit_mov_r32_imm32(&code, 0, 3);            /* mov eax, 3 (sys_close) */
    /* mov rdi, r14 */
    buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xF7);
    emit_syscall(&code);

    /* === jmp accept_loop === */
    buf_write8(&code, 0xE9);
    int jmp_accept_patch = code.len;
    buf_write32(&code, 0);
    patch_rel32(&code, jmp_accept_patch, accept_loop);

    /* Patch accept error jump */
    patch_rel32(&code, accept_err_patch, accept_loop);

    /* === Patch data base lea displacement === */
    /* The lea r13, [rip+disp32] instruction:
     * RIP at the time of lea = code offset right after the lea instruction
     * (which is data_base_patch + 4).
     * The data starts at code.len (right after all code). */
    patch_rel32(&code, data_base_patch, code.len);

    /* Append data section to code */
    buf_write(&code, data.data, data.len);

    /* === Build ELF === */
    /* Need 2 PHDR entries: one for RX code, one for RW data (using same segment
     * for simplicity since we have sockaddr_in that needs to be readable) */
    /* Actually, we can use a single RWX segment since this is a simple binary */
    int phdr_count = 1;
    int elf_code_offset = ELF_HEADER_SIZE + PHDR_SIZE * phdr_count;
    uint64_t file_size = elf_code_offset + code.len;
    uint64_t base_addr = BASE_ADDR;
    uint64_t entry_addr = base_addr + elf_code_offset;

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
    buf_write64(&out, entry_addr);    /* e_entry */
    buf_write64(&out, ELF_HEADER_SIZE); /* e_phoff */
    buf_write64(&out, 0);             /* e_shoff */
    buf_write32(&out, 0);             /* e_flags */
    buf_write16(&out, ELF_HEADER_SIZE); /* e_ehsize */
    buf_write16(&out, PHDR_SIZE);     /* e_phentsize */
    buf_write16(&out, phdr_count);    /* e_phnum */
    buf_write16(&out, 0);             /* e_shentsize */
    buf_write16(&out, 0);             /* e_shnum */
    buf_write16(&out, 0);             /* e_shstrndx */

    /* Program Header: PT_LOAD — RWX (code + data in one segment) */
    buf_write32(&out, 1);             /* PT_LOAD */
    buf_write32(&out, 0x7);           /* PF_R | PF_W | PF_X */
    buf_write64(&out, 0);             /* p_offset */
    buf_write64(&out, base_addr);     /* p_vaddr */
    buf_write64(&out, base_addr);     /* p_paddr */
    buf_write64(&out, file_size);     /* p_filesz */
    buf_write64(&out, file_size + 4112 + 64); /* p_memsz (extra for stack) */
    buf_write64(&out, 0x1000);        /* p_align */

    buf_write(&out, code.data, code.len);

    /* Write to file */
    FILE *f = fopen(output_path, "wb");
    if (!f) {
        diag_error_no_loc("cannot open '%s' for writing", output_path);
    }

    fwrite(out.data, 1, out.len, f);
    fclose(f);
    chmod(output_path, 0755);

    buf_free(&code);
    buf_free(&data);
    buf_free(&out);
    free(resp_offsets);
    free(resp_lengths);
    free(method_offsets);
    free(method_lengths);
    free(path_offsets);
    free(route_match_patches);
    return 0;
}

/* ================================================================
 * emit_net_binary — emit an ELF binary for TCP/UDP networking
 *
 * Four modes:
 *   NET_TCP_LISTEN:  socket → setsockopt → bind → listen → accept loop
 *                    (accept → write response → close → loop)
 *   NET_TCP_CONNECT: socket → connect → write message → read → print → exit
 *   NET_UDP_LISTEN:  socket → bind → recvfrom loop
 *                    (recvfrom → sendto response → loop)
 *   NET_UDP_SEND:    socket → sendto message → recvfrom → print → exit
 *
 * Data section layout (after code):
 *   - response/message string data
 *   - sockaddr_in struct (16 bytes)
 * ================================================================ */

/* Parse IPv4 dotted-decimal string to network-byte-order uint32 at compile time */
static uint32_t parse_ipv4(const char *host) {
    unsigned int a, b, c, d;
    if (sscanf(host, "%u.%u.%u.%u", &a, &b, &c, &d) != 4)
        return 0;
    if (a > 255 || b > 255 || c > 255 || d > 255)
        return 0;
    return (uint32_t)((a) | (b << 8) | (c << 16) | (d << 24));
}

static void emit_elf_with_code(Buffer *code, const char *output_path) {
    int phdr_count = 1;
    int elf_code_offset = ELF_HEADER_SIZE + PHDR_SIZE * phdr_count;
    uint64_t file_size = elf_code_offset + code->len;
    uint64_t base_addr = BASE_ADDR;
    uint64_t entry_addr = base_addr + elf_code_offset;

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
    buf_write64(&out, entry_addr);    /* e_entry */
    buf_write64(&out, ELF_HEADER_SIZE); /* e_phoff */
    buf_write64(&out, 0);             /* e_shoff */
    buf_write32(&out, 0);             /* e_flags */
    buf_write16(&out, ELF_HEADER_SIZE); /* e_ehsize */
    buf_write16(&out, PHDR_SIZE);     /* e_phentsize */
    buf_write16(&out, phdr_count);    /* e_phnum */
    buf_write16(&out, 0);             /* e_shentsize */
    buf_write16(&out, 0);             /* e_shnum */
    buf_write16(&out, 0);             /* e_shstrndx */

    /* Program Header: PT_LOAD — RWX */
    buf_write32(&out, 1);             /* PT_LOAD */
    buf_write32(&out, 0x7);           /* PF_R | PF_W | PF_X */
    buf_write64(&out, 0);             /* p_offset */
    buf_write64(&out, base_addr);     /* p_vaddr */
    buf_write64(&out, base_addr);     /* p_paddr */
    buf_write64(&out, file_size);     /* p_filesz */
    buf_write64(&out, file_size + 8192); /* p_memsz (extra for stack/buffers) */
    buf_write64(&out, 0x1000);        /* p_align */

    buf_write(&out, code->data, code->len);

    FILE *f = fopen(output_path, "wb");
    if (!f)
        diag_error_no_loc("cannot open '%s' for writing", output_path);

    fwrite(out.data, 1, out.len, f);
    fclose(f);
    chmod(output_path, 0755);

    buf_free(&out);
}

int emit_net_binary(NetConfig *config, const char *output_path)
{
    Buffer code;
    buf_init(&code);

    /* Build data section */
    Buffer data;
    buf_init(&data);

    /* Write message/response string */
    int data_str_offset = data.len;
    buf_write(&data, config->data, config->data_len);

    /* Build sockaddr_in (16 bytes) */
    int sockaddr_offset = data.len;
    uint16_t sin_family = 2;  /* AF_INET */
    uint16_t sin_port = (uint16_t)(((config->port & 0xFF) << 8) | ((config->port >> 8) & 0xFF));
    buf_write16(&data, sin_family);
    buf_write16(&data, sin_port);
    if (config->host) {
        uint32_t addr = parse_ipv4(config->host);
        buf_write32(&data, addr);
    } else {
        buf_write32(&data, 0);  /* INADDR_ANY */
    }
    buf_write64(&data, 0);  /* padding */

    /*
     * Register plan:
     *   r12 = socket fd (callee-saved)
     *   r13 = data base address (callee-saved)
     *   r14 = client fd (TCP server accept loop)
     *   Stack: 4096-byte read buffer at [rbp - 4096]
     */

    /* === Prologue === */
    /* push rbp */
    buf_write8(&code, 0x55);
    /* mov rbp, rsp */
    buf_write8(&code, 0x48); buf_write8(&code, 0x89); buf_write8(&code, 0xE5);
    /* sub rsp, 4112 (4096 buffer + 16 alignment) */
    buf_write8(&code, 0x48); buf_write8(&code, 0x81); buf_write8(&code, 0xEC);
    buf_write32(&code, 4112);
    /* push r12 */
    buf_write8(&code, 0x41); buf_write8(&code, 0x54);
    /* push r13 */
    buf_write8(&code, 0x41); buf_write8(&code, 0x55);

    /* === Load data base address into r13 via lea r13, [rip+disp32] === */
    buf_write8(&code, 0x4C); buf_write8(&code, 0x8D); buf_write8(&code, 0x2D);
    int data_base_patch = code.len;
    buf_write32(&code, 0); /* patched later */

    int sock_type;
    switch (config->mode) {
    case NET_TCP_LISTEN:
    case NET_TCP_CONNECT:
        sock_type = 1; /* SOCK_STREAM */
        break;
    case NET_UDP_LISTEN:
    case NET_UDP_SEND:
        sock_type = 2; /* SOCK_DGRAM */
        break;
    }

    /* === socket(AF_INET=2, SOCK_STREAM/DGRAM, 0) === */
    emit_mov_r32_imm32(&code, 0, 41);             /* mov eax, 41 (sys_socket) */
    emit_mov_r32_imm32(&code, 7, 2);              /* mov edi, 2 (AF_INET) */
    emit_mov_r32_imm32(&code, 6, (uint32_t)sock_type);
    /* xor edx, edx */
    buf_write8(&code, 0x31); buf_write8(&code, 0xD2);
    emit_syscall(&code);
    /* mov r12, rax (save fd) */
    buf_write8(&code, 0x49); buf_write8(&code, 0x89); buf_write8(&code, 0xC4);

    switch (config->mode) {

    /* ============================================================
     * NET_TCP_LISTEN
     * ============================================================ */
    case NET_TCP_LISTEN: {
        /* setsockopt(fd, SOL_SOCKET=1, SO_REUSEADDR=2, &1, 4) */
        /* mov dword [rsp], 1 */
        buf_write8(&code, 0xC7); buf_write8(&code, 0x04); buf_write8(&code, 0x24);
        buf_write32(&code, 1);
        emit_mov_r32_imm32(&code, 0, 54);         /* sys_setsockopt */
        /* mov rdi, r12 */
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7);
        emit_mov_r32_imm32(&code, 6, 1);          /* SOL_SOCKET */
        emit_mov_r32_imm32(&code, 2, 2);          /* SO_REUSEADDR */
        /* mov r10, rsp */
        buf_write8(&code, 0x49); buf_write8(&code, 0x89); buf_write8(&code, 0xE2);
        /* mov r8d, 4 */
        buf_write8(&code, 0x41); buf_write8(&code, 0xB8); buf_write32(&code, 4);
        emit_syscall(&code);

        /* bind(fd, &sockaddr, 16) */
        emit_mov_r32_imm32(&code, 0, 49);         /* sys_bind */
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7); /* mov rdi, r12 */
        /* lea rsi, [r13 + sockaddr_offset] */
        buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
        buf_write32(&code, (uint32_t)sockaddr_offset);
        emit_mov_r32_imm32(&code, 2, 16);
        emit_syscall(&code);

        /* Check bind: if negative, exit(1) */
        buf_write8(&code, 0x48); buf_write8(&code, 0x85); buf_write8(&code, 0xC0); /* test rax, rax */
        buf_write8(&code, 0x79);                   /* jns +skip */
        int bind_ok = code.len;
        buf_write8(&code, 0x00);
        emit_mov_r32_imm32(&code, 0, 60);
        emit_mov_r32_imm32(&code, 7, 1);
        emit_syscall(&code);
        code.data[bind_ok] = (uint8_t)(code.len - bind_ok - 1);

        /* listen(fd, 128) */
        emit_mov_r32_imm32(&code, 0, 50);         /* sys_listen */
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7); /* mov rdi, r12 */
        emit_mov_r32_imm32(&code, 6, 128);
        emit_syscall(&code);

        /* accept_loop: */
        int accept_loop = code.len;

        /* accept(fd, NULL, NULL) = syscall 43 */
        emit_mov_r32_imm32(&code, 0, 43);
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7); /* mov rdi, r12 */
        buf_write8(&code, 0x31); buf_write8(&code, 0xF6); /* xor esi, esi */
        buf_write8(&code, 0x31); buf_write8(&code, 0xD2); /* xor edx, edx */
        emit_syscall(&code);

        /* if accept < 0, loop back */
        buf_write8(&code, 0x48); buf_write8(&code, 0x85); buf_write8(&code, 0xC0); /* test rax, rax */
        buf_write8(&code, 0x0F); buf_write8(&code, 0x88);
        int accept_err_patch = code.len;
        buf_write32(&code, 0);

        /* mov r14, rax (client_fd) */
        buf_write8(&code, 0x49); buf_write8(&code, 0x89); buf_write8(&code, 0xC6);

        /* write(client_fd, response, len) = syscall 1 */
        emit_mov_r32_imm32(&code, 0, 1);
        /* mov rdi, r14 */
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xF7);
        /* lea rsi, [r13 + data_str_offset] */
        buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
        buf_write32(&code, (uint32_t)data_str_offset);
        emit_mov_r32_imm32(&code, 2, (uint32_t)config->data_len);
        emit_syscall(&code);

        /* close(client_fd) = syscall 3 */
        emit_mov_r32_imm32(&code, 0, 3);
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xF7); /* mov rdi, r14 */
        emit_syscall(&code);

        /* jmp accept_loop */
        buf_write8(&code, 0xE9);
        int jmp_patch = code.len;
        buf_write32(&code, 0);
        patch_rel32(&code, jmp_patch, accept_loop);

        /* patch accept error to loop back */
        patch_rel32(&code, accept_err_patch, accept_loop);
        break;
    }

    /* ============================================================
     * NET_TCP_CONNECT
     * ============================================================ */
    case NET_TCP_CONNECT: {
        /* connect(fd, &sockaddr, 16) = syscall 42 */
        emit_mov_r32_imm32(&code, 0, 42);
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7); /* mov rdi, r12 */
        /* lea rsi, [r13 + sockaddr_offset] */
        buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
        buf_write32(&code, (uint32_t)sockaddr_offset);
        emit_mov_r32_imm32(&code, 2, 16);
        emit_syscall(&code);

        /* if connect < 0, exit(1) */
        buf_write8(&code, 0x48); buf_write8(&code, 0x85); buf_write8(&code, 0xC0); /* test rax, rax */
        buf_write8(&code, 0x79);
        int conn_ok = code.len;
        buf_write8(&code, 0x00);
        emit_mov_r32_imm32(&code, 0, 60);
        emit_mov_r32_imm32(&code, 7, 1);
        emit_syscall(&code);
        code.data[conn_ok] = (uint8_t)(code.len - conn_ok - 1);

        /* write(fd, message, len) = syscall 1 */
        emit_mov_r32_imm32(&code, 0, 1);
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7); /* mov rdi, r12 */
        /* lea rsi, [r13 + data_str_offset] */
        buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
        buf_write32(&code, (uint32_t)data_str_offset);
        emit_mov_r32_imm32(&code, 2, (uint32_t)config->data_len);
        emit_syscall(&code);

        /* read(fd, buf, 4096) = syscall 0 */
        emit_mov_r32_imm32(&code, 0, 0);
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7); /* mov rdi, r12 */
        /* lea rsi, [rbp - 4096] */
        buf_write8(&code, 0x48); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
        buf_write32(&code, (uint32_t)(-4096));
        emit_mov_r32_imm32(&code, 2, 4096);
        emit_syscall(&code);

        /* if read <= 0, skip print */
        buf_write8(&code, 0x48); buf_write8(&code, 0x85); buf_write8(&code, 0xC0); /* test rax, rax */
        buf_write8(&code, 0x0F); buf_write8(&code, 0x8E);
        int read_skip = code.len;
        buf_write32(&code, 0);

        /* write(stdout=1, buf, bytes_read) = syscall 1 */
        /* mov rdx, rax (bytes_read) */
        buf_write8(&code, 0x48); buf_write8(&code, 0x89); buf_write8(&code, 0xC2);
        emit_mov_r32_imm32(&code, 0, 1);
        emit_mov_r32_imm32(&code, 7, 1);          /* stdout */
        /* lea rsi, [rbp - 4096] */
        buf_write8(&code, 0x48); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
        buf_write32(&code, (uint32_t)(-4096));
        emit_syscall(&code);

        /* skip_print: */
        int skip_print = code.len;
        patch_rel32(&code, read_skip, skip_print);

        /* close(fd) = syscall 3 */
        emit_mov_r32_imm32(&code, 0, 3);
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7); /* mov rdi, r12 */
        emit_syscall(&code);

        /* exit(0) */
        emit_mov_r32_imm32(&code, 0, 60);
        buf_write8(&code, 0x31); buf_write8(&code, 0xFF); /* xor edi, edi */
        emit_syscall(&code);
        break;
    }

    /* ============================================================
     * NET_UDP_LISTEN
     * ============================================================ */
    case NET_UDP_LISTEN: {
        /* bind(fd, &sockaddr, 16) = syscall 49 */
        emit_mov_r32_imm32(&code, 0, 49);
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7); /* mov rdi, r12 */
        buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
        buf_write32(&code, (uint32_t)sockaddr_offset);
        emit_mov_r32_imm32(&code, 2, 16);
        emit_syscall(&code);

        /* Check bind */
        buf_write8(&code, 0x48); buf_write8(&code, 0x85); buf_write8(&code, 0xC0);
        buf_write8(&code, 0x79);
        int bind_ok = code.len;
        buf_write8(&code, 0x00);
        emit_mov_r32_imm32(&code, 0, 60);
        emit_mov_r32_imm32(&code, 7, 1);
        emit_syscall(&code);
        code.data[bind_ok] = (uint8_t)(code.len - bind_ok - 1);

        /* recvfrom_loop: */
        int recvfrom_loop = code.len;

        /*
         * recvfrom(fd, buf, 4096, 0, &peer_addr, &addrlen) = syscall 45
         * We store peer_addr at [rbp - 4096 - 16] and addrlen at [rbp - 4096 - 20]
         * So peer_addr at [rbp - 4112] (16 bytes), addrlen at [rbp - 4128] (4 bytes)
         * Actually let's use: buf at [rbp-4096], peer_addr at [rbp-4112], addrlen at [rbp-4116]
         * We already have sub rsp, 4112. Let's use space below rbp more carefully:
         *   [rbp - 4096] = recv buffer (4096 bytes)
         *   [rbp - 4112] = peer_addr (16 bytes)
         *   [rbp - 4116] = addrlen (4 bytes)
         * We need rsp to be far enough. sub rsp, 4112 gives us room up to [rbp - 4112]
         * Let's extend the stack: sub rsp, 4128 would be better.
         * But we already emitted the prologue. We can just sub rsp a bit more.
         */
        /* sub rsp, 32 (extra room for peer_addr + addrlen) */
        buf_write8(&code, 0x48); buf_write8(&code, 0x83); buf_write8(&code, 0xEC);
        buf_write8(&code, 32);

        /* Store addrlen = 16 at [rbp - 4116] */
        /* mov dword [rbp - 4116], 16 */
        buf_write8(&code, 0xC7); buf_write8(&code, 0x85);
        buf_write32(&code, (uint32_t)(-4116));
        buf_write32(&code, 16);

        /* recvfrom(fd, buf, 4096, 0, &peer_addr, &addrlen) */
        emit_mov_r32_imm32(&code, 0, 45);         /* sys_recvfrom */
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7); /* mov rdi, r12 */
        /* lea rsi, [rbp - 4096] */
        buf_write8(&code, 0x48); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
        buf_write32(&code, (uint32_t)(-4096));
        emit_mov_r32_imm32(&code, 2, 4096);       /* len */
        /* xor r10d, r10d (flags = 0) */
        buf_write8(&code, 0x45); buf_write8(&code, 0x31); buf_write8(&code, 0xD2);
        /* lea r8, [rbp - 4112]  (peer_addr) */
        buf_write8(&code, 0x4C); buf_write8(&code, 0x8D); buf_write8(&code, 0x85);
        buf_write32(&code, (uint32_t)(-4112));
        /* lea r9, [rbp - 4116]  (addrlen) */
        buf_write8(&code, 0x4C); buf_write8(&code, 0x8D); buf_write8(&code, 0x8D);
        buf_write32(&code, (uint32_t)(-4116));
        emit_syscall(&code);

        /* if recvfrom <= 0, loop back (ignore errors) */
        buf_write8(&code, 0x48); buf_write8(&code, 0x85); buf_write8(&code, 0xC0);
        buf_write8(&code, 0x0F); buf_write8(&code, 0x8E);
        int recv_err_patch = code.len;
        buf_write32(&code, 0);

        /*
         * sendto(fd, response, len, 0, &peer_addr, addrlen) = syscall 44
         * rdi = fd, rsi = data, rdx = len, r10 = flags(0), r8 = addr, r9 = addrlen
         */
        emit_mov_r32_imm32(&code, 0, 44);         /* sys_sendto */
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7); /* mov rdi, r12 */
        /* lea rsi, [r13 + data_str_offset] */
        buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
        buf_write32(&code, (uint32_t)data_str_offset);
        emit_mov_r32_imm32(&code, 2, (uint32_t)config->data_len);
        /* xor r10d, r10d (flags = 0) */
        buf_write8(&code, 0x45); buf_write8(&code, 0x31); buf_write8(&code, 0xD2);
        /* lea r8, [rbp - 4112] (peer_addr) */
        buf_write8(&code, 0x4C); buf_write8(&code, 0x8D); buf_write8(&code, 0x85);
        buf_write32(&code, (uint32_t)(-4112));
        /* mov r9d, 16 */
        buf_write8(&code, 0x41); buf_write8(&code, 0xB9); buf_write32(&code, 16);
        emit_syscall(&code);

        /* Restore stack and loop */
        buf_write8(&code, 0x48); buf_write8(&code, 0x83); buf_write8(&code, 0xC4);
        buf_write8(&code, 32); /* add rsp, 32 */

        /* jmp recvfrom_loop */
        buf_write8(&code, 0xE9);
        int jmp_patch = code.len;
        buf_write32(&code, 0);
        patch_rel32(&code, jmp_patch, recvfrom_loop);

        /* patch recv error: restore stack and loop */
        int recv_err_target = code.len;
        buf_write8(&code, 0x48); buf_write8(&code, 0x83); buf_write8(&code, 0xC4);
        buf_write8(&code, 32); /* add rsp, 32 */
        buf_write8(&code, 0xE9);
        int jmp_patch2 = code.len;
        buf_write32(&code, 0);
        patch_rel32(&code, jmp_patch2, recvfrom_loop);
        patch_rel32(&code, recv_err_patch, recv_err_target);
        break;
    }

    /* ============================================================
     * NET_UDP_SEND
     * ============================================================ */
    case NET_UDP_SEND: {
        /*
         * sendto(fd, message, len, 0, &dest_addr, 16) = syscall 44
         */
        emit_mov_r32_imm32(&code, 0, 44);
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7); /* mov rdi, r12 */
        /* lea rsi, [r13 + data_str_offset] */
        buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
        buf_write32(&code, (uint32_t)data_str_offset);
        emit_mov_r32_imm32(&code, 2, (uint32_t)config->data_len);
        /* xor r10d, r10d (flags = 0) */
        buf_write8(&code, 0x45); buf_write8(&code, 0x31); buf_write8(&code, 0xD2);
        /* lea r8, [r13 + sockaddr_offset] */
        buf_write8(&code, 0x4D); buf_write8(&code, 0x8D); buf_write8(&code, 0x85);
        buf_write32(&code, (uint32_t)sockaddr_offset);
        /* mov r9d, 16 */
        buf_write8(&code, 0x41); buf_write8(&code, 0xB9); buf_write32(&code, 16);
        emit_syscall(&code);

        /* recvfrom(fd, buf, 4096, 0, NULL, NULL) = syscall 45 */
        emit_mov_r32_imm32(&code, 0, 45);
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7); /* mov rdi, r12 */
        /* lea rsi, [rbp - 4096] */
        buf_write8(&code, 0x48); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
        buf_write32(&code, (uint32_t)(-4096));
        emit_mov_r32_imm32(&code, 2, 4096);
        /* xor r10d, r10d */
        buf_write8(&code, 0x45); buf_write8(&code, 0x31); buf_write8(&code, 0xD2);
        /* xor r8d, r8d (NULL) */
        buf_write8(&code, 0x45); buf_write8(&code, 0x31); buf_write8(&code, 0xC0);
        /* xor r9d, r9d (NULL) */
        buf_write8(&code, 0x45); buf_write8(&code, 0x31); buf_write8(&code, 0xC9);
        emit_syscall(&code);

        /* if recvfrom <= 0, skip print */
        buf_write8(&code, 0x48); buf_write8(&code, 0x85); buf_write8(&code, 0xC0);
        buf_write8(&code, 0x0F); buf_write8(&code, 0x8E);
        int recv_skip = code.len;
        buf_write32(&code, 0);

        /* write(stdout, buf, bytes_read) */
        /* mov rdx, rax */
        buf_write8(&code, 0x48); buf_write8(&code, 0x89); buf_write8(&code, 0xC2);
        emit_mov_r32_imm32(&code, 0, 1);          /* sys_write */
        emit_mov_r32_imm32(&code, 7, 1);          /* stdout */
        buf_write8(&code, 0x48); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
        buf_write32(&code, (uint32_t)(-4096));
        emit_syscall(&code);

        int skip_print = code.len;
        patch_rel32(&code, recv_skip, skip_print);

        /* close(fd) */
        emit_mov_r32_imm32(&code, 0, 3);
        buf_write8(&code, 0x4C); buf_write8(&code, 0x89); buf_write8(&code, 0xE7);
        emit_syscall(&code);

        /* exit(0) */
        emit_mov_r32_imm32(&code, 0, 60);
        buf_write8(&code, 0x31); buf_write8(&code, 0xFF);
        emit_syscall(&code);
        break;
    }
    } /* end switch */

    /* Patch data base address */
    patch_rel32(&code, data_base_patch, code.len);

    /* Append data */
    buf_write(&code, data.data, data.len);

    /* Emit ELF */
    emit_elf_with_code(&code, output_path);

    buf_free(&code);
    buf_free(&data);
    return 0;
}

/* ================================================================
 * emit_binary_ir — emit an ELF binary from IR instructions
 *
 * Stack frame layout:
 *   [rbp + 0]           saved rbp
 *   [rbp - 8]           local slot 0
 *   [rbp - 16]          local slot 1
 *   ...
 *   [rbp - 8*N]         local slot N-1
 *   [rbp - 8*(N+1)]     vreg temp 0
 *   [rbp - 8*(N+2)]     vreg temp 1
 *   ...
 *
 * All vregs and locals live on the stack (no register allocator).
 * Expressions use rax, rcx, rdx as temporaries.
 * ================================================================ */

/* Helper: offset from rbp for a vreg */
static int vreg_offset(int vreg, int slot_count) {
    return -8 * (slot_count + vreg + 1);
}

/* Helper: offset from rbp for a local slot */
static int slot_offset(int slot) {
    return -8 * (slot + 1);
}

/* Helper: emit mov rax, [rbp + disp32] */
static void emit_load_rbp(Buffer *c, int disp) {
    /* mov rax, [rbp + disp32] */
    buf_write8(c, 0x48); buf_write8(c, 0x8B); buf_write8(c, 0x85);
    buf_write32(c, (uint32_t)(int32_t)disp);
}

/* Helper: emit mov [rbp + disp32], rax */
static void emit_store_rbp_rax(Buffer *c, int disp) {
    /* mov [rbp + disp32], rax */
    buf_write8(c, 0x48); buf_write8(c, 0x89); buf_write8(c, 0x85);
    buf_write32(c, (uint32_t)(int32_t)disp);
}

/* Helper: emit mov rcx, [rbp + disp32] */
static void emit_load_rbp_rcx(Buffer *c, int disp) {
    /* mov rcx, [rbp + disp32] */
    buf_write8(c, 0x48); buf_write8(c, 0x8B); buf_write8(c, 0x8D);
    buf_write32(c, (uint32_t)(int32_t)disp);
}

/* Helper: emit mov rax, imm64 */
static void emit_mov_rax_imm64(Buffer *c, int64_t imm) {
    /* movabs rax, imm64 */
    buf_write8(c, 0x48); buf_write8(c, 0xB8);
    buf_write64(c, (uint64_t)imm);
}

int emit_binary_ir(IRProgram *prog, const char *output_path)
{
    int slot_count = prog->next_slot;
    int vreg_count = prog->next_vreg;
    int total_slots = slot_count + vreg_count;
    /* Align frame size to 16 bytes */
    int frame_size = ((total_slots * 8) + 15) & ~15;
    if (frame_size < 16) frame_size = 16;

    Buffer code;
    buf_init(&code);

    Buffer data;
    buf_init(&data);

    /* Build string data section and record offsets */
    int *str_data_offsets = malloc(prog->string_count * sizeof(int));
    for (int i = 0; i < prog->string_count; i++) {
        str_data_offsets[i] = data.len;
        buf_write(&data, prog->strings[i].data, prog->strings[i].len);
    }

    /* Add "true" and "false" strings for IR_PRINT_BOOL */
    int bool_true_offset = data.len;
    buf_write(&data, "true", 4);
    int bool_false_offset = data.len;
    buf_write(&data, "false", 5);

    /* Reserve space for label offsets — we'll patch jumps after code gen */
    int *label_offsets = calloc(prog->next_label, sizeof(int));

    /* Track jump instructions that need patching */
    typedef struct { int code_offset; int label_id; } JmpPatch;
    JmpPatch *patches = malloc(prog->instr_count * sizeof(JmpPatch));
    int patch_count = 0;

    /* === Prologue === */
    /* push rbp */
    buf_write8(&code, 0x55);
    /* mov rbp, rsp */
    buf_write8(&code, 0x48); buf_write8(&code, 0x89); buf_write8(&code, 0xE5);
    /* sub rsp, frame_size */
    buf_write8(&code, 0x48); buf_write8(&code, 0x81); buf_write8(&code, 0xEC);
    buf_write32(&code, (uint32_t)frame_size);

    /* Load data base address into r13 */
    buf_write8(&code, 0x4C); buf_write8(&code, 0x8D); buf_write8(&code, 0x2D);
    int data_base_patch = code.len;
    buf_write32(&code, 0); /* patched after code gen */

    /* === Lower each IR instruction === */
    for (int i = 0; i < prog->instr_count; i++) {
        IRInstr *ir = &prog->instrs[i];

        switch (ir->op) {

        case IR_CONST_INT: {
            /* mov rax, imm64; mov [rbp + vreg_off], rax */
            emit_mov_rax_imm64(&code, ir->imm);
            emit_store_rbp_rax(&code, vreg_offset(ir->dst, slot_count));
            break;
        }

        case IR_CONST_STR:
            /* String data is in the data section; we just record the index.
             * This opcode doesn't produce a runtime value used by other
             * instructions (strings are only used by IR_PRINT_STR). */
            break;

        case IR_LOAD_LOCAL: {
            /* mov rax, [rbp + slot_off]; mov [rbp + vreg_off], rax */
            emit_load_rbp(&code, slot_offset(ir->slot));
            emit_store_rbp_rax(&code, vreg_offset(ir->dst, slot_count));
            break;
        }

        case IR_STORE_LOCAL: {
            /* mov rax, [rbp + src_vreg_off]; mov [rbp + slot_off], rax */
            emit_load_rbp(&code, vreg_offset(ir->src, slot_count));
            emit_store_rbp_rax(&code, slot_offset(ir->slot));
            break;
        }

        case IR_ADD: case IR_SUB: case IR_MUL:
        case IR_BIT_AND: case IR_BIT_OR: case IR_BIT_XOR:
        case IR_SHL: case IR_SHR: {
            /* mov rax, [rbp + lhs]; mov rcx, [rbp + rhs]; OP rax, rcx; mov [rbp + dst], rax */
            emit_load_rbp(&code, vreg_offset(ir->lhs, slot_count));
            emit_load_rbp_rcx(&code, vreg_offset(ir->rhs, slot_count));

            switch (ir->op) {
            case IR_ADD:
                /* add rax, rcx */
                buf_write8(&code, 0x48); buf_write8(&code, 0x01); buf_write8(&code, 0xC8);
                break;
            case IR_SUB:
                /* sub rax, rcx */
                buf_write8(&code, 0x48); buf_write8(&code, 0x29); buf_write8(&code, 0xC8);
                break;
            case IR_MUL:
                /* imul rax, rcx */
                buf_write8(&code, 0x48); buf_write8(&code, 0x0F); buf_write8(&code, 0xAF); buf_write8(&code, 0xC1);
                break;
            case IR_BIT_AND:
                /* and rax, rcx */
                buf_write8(&code, 0x48); buf_write8(&code, 0x21); buf_write8(&code, 0xC8);
                break;
            case IR_BIT_OR:
                /* or rax, rcx */
                buf_write8(&code, 0x48); buf_write8(&code, 0x09); buf_write8(&code, 0xC8);
                break;
            case IR_BIT_XOR:
                /* xor rax, rcx */
                buf_write8(&code, 0x48); buf_write8(&code, 0x31); buf_write8(&code, 0xC8);
                break;
            case IR_SHL:
                /* shl rax, cl */
                buf_write8(&code, 0x48); buf_write8(&code, 0xD3); buf_write8(&code, 0xE0);
                break;
            case IR_SHR:
                /* sar rax, cl (arithmetic shift right) */
                buf_write8(&code, 0x48); buf_write8(&code, 0xD3); buf_write8(&code, 0xF8);
                break;
            default: break;
            }

            emit_store_rbp_rax(&code, vreg_offset(ir->dst, slot_count));
            break;
        }

        case IR_DIV: case IR_MOD: {
            /* mov rax, [rbp + lhs]; cqo; mov rcx, [rbp + rhs]; idiv rcx */
            emit_load_rbp(&code, vreg_offset(ir->lhs, slot_count));
            /* cqo (sign-extend rax into rdx:rax) */
            buf_write8(&code, 0x48); buf_write8(&code, 0x99);
            emit_load_rbp_rcx(&code, vreg_offset(ir->rhs, slot_count));
            /* idiv rcx */
            buf_write8(&code, 0x48); buf_write8(&code, 0xF7); buf_write8(&code, 0xF9);

            if (ir->op == IR_DIV) {
                /* result in rax */
                emit_store_rbp_rax(&code, vreg_offset(ir->dst, slot_count));
            } else {
                /* remainder in rdx — mov rax, rdx first */
                buf_write8(&code, 0x48); buf_write8(&code, 0x89); buf_write8(&code, 0xD0);
                emit_store_rbp_rax(&code, vreg_offset(ir->dst, slot_count));
            }
            break;
        }

        case IR_NEG: {
            /* mov rax, [rbp + src]; neg rax; mov [rbp + dst], rax */
            emit_load_rbp(&code, vreg_offset(ir->src, slot_count));
            /* neg rax */
            buf_write8(&code, 0x48); buf_write8(&code, 0xF7); buf_write8(&code, 0xD8);
            emit_store_rbp_rax(&code, vreg_offset(ir->dst, slot_count));
            break;
        }

        case IR_BIT_NOT: {
            /* mov rax, [rbp + src]; not rax; mov [rbp + dst], rax */
            emit_load_rbp(&code, vreg_offset(ir->src, slot_count));
            /* not rax */
            buf_write8(&code, 0x48); buf_write8(&code, 0xF7); buf_write8(&code, 0xD0);
            emit_store_rbp_rax(&code, vreg_offset(ir->dst, slot_count));
            break;
        }

        case IR_CMP_EQ: case IR_CMP_NE:
        case IR_CMP_LT: case IR_CMP_LE:
        case IR_CMP_GT: case IR_CMP_GE: {
            /* mov rax, [rbp + lhs]; cmp rax, [rbp + rhs]; setCC al; movzx rax, al */
            emit_load_rbp(&code, vreg_offset(ir->lhs, slot_count));
            emit_load_rbp_rcx(&code, vreg_offset(ir->rhs, slot_count));
            /* cmp rax, rcx */
            buf_write8(&code, 0x48); buf_write8(&code, 0x39); buf_write8(&code, 0xC8);

            /* setCC al */
            buf_write8(&code, 0x0F);
            switch (ir->op) {
            case IR_CMP_EQ: buf_write8(&code, 0x94); break; /* sete al */
            case IR_CMP_NE: buf_write8(&code, 0x95); break; /* setne al */
            case IR_CMP_LT: buf_write8(&code, 0x9C); break; /* setl al */
            case IR_CMP_LE: buf_write8(&code, 0x9E); break; /* setle al */
            case IR_CMP_GT: buf_write8(&code, 0x9F); break; /* setg al */
            case IR_CMP_GE: buf_write8(&code, 0x9D); break; /* setge al */
            default: buf_write8(&code, 0x94); break;
            }
            buf_write8(&code, 0xC0); /* ModR/M: al */

            /* movzx rax, al */
            buf_write8(&code, 0x48); buf_write8(&code, 0x0F); buf_write8(&code, 0xB6); buf_write8(&code, 0xC0);

            emit_store_rbp_rax(&code, vreg_offset(ir->dst, slot_count));
            break;
        }

        case IR_LABEL:
            label_offsets[ir->label_id] = code.len;
            break;

        case IR_JMP:
            /* jmp rel32 — will be patched */
            buf_write8(&code, 0xE9);
            patches[patch_count].code_offset = code.len;
            patches[patch_count].label_id = ir->label_id;
            patch_count++;
            buf_write32(&code, 0); /* placeholder */
            break;

        case IR_JZ: {
            /* mov rax, [rbp + src]; test rax, rax; jz rel32 */
            emit_load_rbp(&code, vreg_offset(ir->src, slot_count));
            /* test rax, rax */
            buf_write8(&code, 0x48); buf_write8(&code, 0x85); buf_write8(&code, 0xC0);
            /* je rel32 */
            buf_write8(&code, 0x0F); buf_write8(&code, 0x84);
            patches[patch_count].code_offset = code.len;
            patches[patch_count].label_id = ir->label_id;
            patch_count++;
            buf_write32(&code, 0);
            break;
        }

        case IR_JNZ: {
            /* mov rax, [rbp + src]; test rax, rax; jnz rel32 */
            emit_load_rbp(&code, vreg_offset(ir->src, slot_count));
            buf_write8(&code, 0x48); buf_write8(&code, 0x85); buf_write8(&code, 0xC0);
            /* jne rel32 */
            buf_write8(&code, 0x0F); buf_write8(&code, 0x85);
            patches[patch_count].code_offset = code.len;
            patches[patch_count].label_id = ir->label_id;
            patch_count++;
            buf_write32(&code, 0);
            break;
        }

        case IR_PRINT_STR: {
            /* write(1, &str_data, len) via syscall */
            /* lea rsi, [r13 + str_offset] */
            buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
            buf_write32(&code, (uint32_t)str_data_offsets[ir->str_idx]);
            /* mov eax, 1 (sys_write) */
            emit_mov_r32_imm32(&code, 0, 1);
            /* mov edi, 1 (stdout) */
            emit_mov_r32_imm32(&code, 7, 1);
            /* mov edx, len */
            emit_mov_r32_imm32(&code, 2, (uint32_t)ir->str_len);
            emit_syscall(&code);
            break;
        }

        case IR_PRINT_INT: {
            /* Load value into rdi, call itoa_print subroutine */
            /* mov rdi, [rbp + src_vreg_off] */
            buf_write8(&code, 0x48); buf_write8(&code, 0x8B); buf_write8(&code, 0xBD);
            buf_write32(&code, (uint32_t)(int32_t)vreg_offset(ir->src, slot_count));
            /* call itoa_print (rel32 — patched later) */
            buf_write8(&code, 0xE8);
            /* We'll record this position and patch it after we emit the subroutine */
            patches[patch_count].code_offset = code.len;
            patches[patch_count].label_id = -1; /* special: itoa */
            patch_count++;
            buf_write32(&code, 0);
            break;
        }

        case IR_PRINT_BOOL: {
            /* Load vreg value, test, branch to print "true" or "false" */
            emit_load_rbp(&code, vreg_offset(ir->src, slot_count));
            /* test rax, rax */
            buf_write8(&code, 0x48); buf_write8(&code, 0x85); buf_write8(&code, 0xC0);
            /* jz print_false (short jump, patched below) */
            buf_write8(&code, 0x74);
            int jz_false_patch = code.len;
            buf_write8(&code, 0x00); /* placeholder */

            /* === print "true" (4 bytes) === */
            /* lea rsi, [r13 + bool_true_offset] */
            buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
            buf_write32(&code, (uint32_t)bool_true_offset);
            emit_mov_r32_imm32(&code, 0, 1);  /* sys_write */
            emit_mov_r32_imm32(&code, 7, 1);  /* stdout */
            emit_mov_r32_imm32(&code, 2, 4);  /* len = 4 */
            emit_syscall(&code);
            /* jmp end_print_bool (short) */
            buf_write8(&code, 0xEB);
            int jmp_end_patch = code.len;
            buf_write8(&code, 0x00); /* placeholder */

            /* === print "false" (5 bytes) === */
            int false_offset_code = code.len;
            code.data[jz_false_patch] = (uint8_t)(false_offset_code - jz_false_patch - 1);

            /* lea rsi, [r13 + bool_false_offset] */
            buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0xB5);
            buf_write32(&code, (uint32_t)bool_false_offset);
            emit_mov_r32_imm32(&code, 0, 1);  /* sys_write */
            emit_mov_r32_imm32(&code, 7, 1);  /* stdout */
            emit_mov_r32_imm32(&code, 2, 5);  /* len = 5 */
            emit_syscall(&code);

            /* end_print_bool: */
            int end_print_bool = code.len;
            code.data[jmp_end_patch] = (uint8_t)(end_print_bool - jmp_end_patch - 1);
            break;
        }

        case IR_EXIT: {
            /* mov eax, 60; xor edi, edi; syscall */
            emit_mov_r32_imm32(&code, 0, 60);
            buf_write8(&code, 0x31); buf_write8(&code, 0xFF);
            emit_syscall(&code);
            break;
        }

        } /* end switch */
    }

    /* === itoa_print subroutine ===
     *
     * Input: rdi = signed 64-bit integer
     * Clobbers: rax, rcx, rdx, rsi, r8
     * Writes the decimal representation to stdout, then returns.
     *
     * Algorithm:
     *   1. Handle negative: if rdi < 0, write '-', negate rdi
     *   2. Build digits on stack buffer (32 bytes) from right to left
     *   3. write(1, buf, len)
     *   4. ret
     */
    int itoa_offset = code.len;

    /* sub rsp, 32 (local buffer) */
    buf_write8(&code, 0x48); buf_write8(&code, 0x83); buf_write8(&code, 0xEC); buf_write8(&code, 32);

    /* mov rax, rdi (value to convert) */
    buf_write8(&code, 0x48); buf_write8(&code, 0x89); buf_write8(&code, 0xF8);

    /* lea r8, [rsp + 31] (pointer to end of buffer) */
    buf_write8(&code, 0x4C); buf_write8(&code, 0x8D); buf_write8(&code, 0x44); buf_write8(&code, 0x24); buf_write8(&code, 31);

    /* mov byte [r8], 0 (null terminator — not used, just a sentinel) */

    /* xor ecx, ecx (digit count = 0) */
    buf_write8(&code, 0x31); buf_write8(&code, 0xC9);

    /* test rax, rax */
    buf_write8(&code, 0x48); buf_write8(&code, 0x85); buf_write8(&code, 0xC0);

    /* jns positive (if >= 0, skip negation) */
    buf_write8(&code, 0x79);
    int jns_patch = code.len;
    buf_write8(&code, 0x00); /* placeholder */

    /* === negative path === */
    /* Save the sign: push 1 onto... actually let's use r9 as sign flag */
    /* neg rax */
    buf_write8(&code, 0x48); buf_write8(&code, 0xF7); buf_write8(&code, 0xD8);

    /* mov r9d, 1 (sign flag) */
    buf_write8(&code, 0x41); buf_write8(&code, 0xB9); buf_write32(&code, 1);

    /* jmp digit_loop */
    buf_write8(&code, 0xEB);
    int jmp_to_loop_patch = code.len;
    buf_write8(&code, 0x00); /* placeholder */

    /* === positive path === */
    int positive_offset = code.len;
    code.data[jns_patch] = (uint8_t)(positive_offset - jns_patch - 1);

    /* xor r9d, r9d (sign flag = 0, positive) */
    buf_write8(&code, 0x45); buf_write8(&code, 0x31); buf_write8(&code, 0xC9);

    /* === digit_loop === */
    int digit_loop = code.len;
    code.data[jmp_to_loop_patch] = (uint8_t)(digit_loop - jmp_to_loop_patch - 1);

    /* Handle special case: value == 0 */
    /* test rax, rax */
    buf_write8(&code, 0x48); buf_write8(&code, 0x85); buf_write8(&code, 0xC0);
    /* jnz do_divide */
    buf_write8(&code, 0x75);
    int jnz_do_div_patch = code.len;
    buf_write8(&code, 0x00);

    /* If rax == 0 and ecx == 0, we need to output "0" */
    /* test ecx, ecx */
    buf_write8(&code, 0x85); buf_write8(&code, 0xC9);
    /* jnz done_digits (we have digits, rax just became 0) */
    buf_write8(&code, 0x75);
    int jnz_done_patch = code.len;
    buf_write8(&code, 0x00);

    /* Write '0' */
    /* mov byte [r8], '0' */
    buf_write8(&code, 0x41); buf_write8(&code, 0xC6); buf_write8(&code, 0x00); buf_write8(&code, '0');
    /* dec r8 */
    buf_write8(&code, 0x49); buf_write8(&code, 0xFF); buf_write8(&code, 0xC8);
    /* inc ecx */
    buf_write8(&code, 0xFF); buf_write8(&code, 0xC1);
    /* jmp done_digits */
    buf_write8(&code, 0xEB);
    int jmp_done2_patch = code.len;
    buf_write8(&code, 0x00);

    /* === do_divide === */
    int do_divide = code.len;
    code.data[jnz_do_div_patch] = (uint8_t)(do_divide - jnz_do_div_patch - 1);

    /* xor edx, edx */
    buf_write8(&code, 0x31); buf_write8(&code, 0xD2);
    /* mov rsi, 10 */
    buf_write8(&code, 0x48); buf_write8(&code, 0xC7); buf_write8(&code, 0xC6); buf_write32(&code, 10);
    /* div rsi (rax = quotient, rdx = remainder) */
    buf_write8(&code, 0x48); buf_write8(&code, 0xF7); buf_write8(&code, 0xF6);
    /* add dl, '0' */
    buf_write8(&code, 0x80); buf_write8(&code, 0xC2); buf_write8(&code, '0');
    /* mov [r8], dl */
    buf_write8(&code, 0x41); buf_write8(&code, 0x88); buf_write8(&code, 0x10);
    /* dec r8 */
    buf_write8(&code, 0x49); buf_write8(&code, 0xFF); buf_write8(&code, 0xC8);
    /* inc ecx */
    buf_write8(&code, 0xFF); buf_write8(&code, 0xC1);
    /* jmp digit_loop */
    buf_write8(&code, 0xEB);
    int jmp_loop_patch = code.len;
    buf_write8(&code, 0x00);
    code.data[jmp_loop_patch] = (uint8_t)((digit_loop - jmp_loop_patch - 1) & 0xFF);

    /* === done_digits === */
    int done_digits = code.len;
    code.data[jnz_done_patch] = (uint8_t)(done_digits - jnz_done_patch - 1);
    code.data[jmp_done2_patch] = (uint8_t)(done_digits - jmp_done2_patch - 1);

    /* If sign flag set, prepend '-' */
    /* test r9d, r9d */
    buf_write8(&code, 0x45); buf_write8(&code, 0x85); buf_write8(&code, 0xC9);
    /* jz no_sign */
    buf_write8(&code, 0x74);
    int jz_no_sign_patch = code.len;
    buf_write8(&code, 0x00);

    /* mov byte [r8], '-' */
    buf_write8(&code, 0x41); buf_write8(&code, 0xC6); buf_write8(&code, 0x00); buf_write8(&code, '-');
    /* dec r8 */
    buf_write8(&code, 0x49); buf_write8(&code, 0xFF); buf_write8(&code, 0xC8);
    /* inc ecx */
    buf_write8(&code, 0xFF); buf_write8(&code, 0xC1);

    /* === no_sign === */
    int no_sign = code.len;
    code.data[jz_no_sign_patch] = (uint8_t)(no_sign - jz_no_sign_patch - 1);

    /* r8 points one before the first char. rsi = r8 + 1, rdx = ecx */
    /* lea rsi, [r8 + 1] */
    buf_write8(&code, 0x49); buf_write8(&code, 0x8D); buf_write8(&code, 0x70); buf_write8(&code, 0x01);
    /* mov edx, ecx */
    buf_write8(&code, 0x89); buf_write8(&code, 0xCA);

    /* write(1, rsi, rdx) */
    emit_mov_r32_imm32(&code, 0, 1);  /* sys_write */
    emit_mov_r32_imm32(&code, 7, 1);  /* stdout */
    emit_syscall(&code);

    /* add rsp, 32 */
    buf_write8(&code, 0x48); buf_write8(&code, 0x83); buf_write8(&code, 0xC4); buf_write8(&code, 32);

    /* ret */
    buf_write8(&code, 0xC3);

    /* === Patch all jumps === */
    for (int p = 0; p < patch_count; p++) {
        if (patches[p].label_id == -1) {
            /* itoa call */
            patch_rel32(&code, patches[p].code_offset, itoa_offset);
        } else {
            patch_rel32(&code, patches[p].code_offset, label_offsets[patches[p].label_id]);
        }
    }

    /* Patch data base lea */
    patch_rel32(&code, data_base_patch, code.len);

    /* Append data */
    buf_write(&code, data.data, data.len);

    /* === Build ELF === */
    emit_elf_with_code(&code, output_path);

    buf_free(&code);
    buf_free(&data);
    free(str_data_offsets);
    free(label_offsets);
    free(patches);
    return 0;
}

#endif
