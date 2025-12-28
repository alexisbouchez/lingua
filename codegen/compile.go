package codegen

import "github.com/alexisbouchez/lingua/parser"

// WASM opcodes
const (
	OpBlock     = 0x02
	OpLoop      = 0x03
	OpIf        = 0x04
	OpElse      = 0x05
	OpEnd       = 0x0b
	OpBr        = 0x0c
	OpBrIf      = 0x0d
	OpCall      = 0x10
	OpLocalGet  = 0x20
	OpLocalSet  = 0x21
	OpLocalTee  = 0x22
	OpGlobalGet = 0x23
	OpGlobalSet = 0x24
	OpI32Eqz    = 0x45
	OpI32Eq     = 0x46
	OpI32Ne     = 0x47
	OpI32LtS    = 0x48
	OpI32GtS    = 0x4a
	OpI32LeS    = 0x4c
	OpI32GeS    = 0x4e
	OpI32Add    = 0x6a
	OpI32Sub    = 0x6b
	OpI32Mul    = 0x6c
	OpI32Div    = 0x6d

	OpI32Load  = 0x28
	OpI32Store = 0x36
)

type Compiler struct {
	fn            *parser.FnDecl
	locals        map[string]int
	numLocals     int
	funcIdx       map[string]int
	globalIdx     map[string]int
	strings       *StringTable
	loopDepth     int // depth of nested loops
	breakLabel    int // label offset for break (to outer block)
	continueLabel int // label offset for continue (to loop)
}

type StringTable struct {
	data   []byte
	offset int // starting offset in memory
}

func NewStringTable(offset int) *StringTable {
	return &StringTable{offset: offset}
}

func (st *StringTable) Add(s string) int {
	addr := st.offset + len(st.data)
	st.data = append(st.data, []byte(s)...)
	return addr
}

func (st *StringTable) Bytes() []byte {
	return st.data
}

// generatePrintlnHelper generates the _println helper function bytecode
// Prints an integer followed by a newline
// Uses memory address 600 for newline character
func generatePrintlnHelper(printIntIdx, fdWriteIdx int) []byte {
	var code []byte

	// Call _print_int(n)
	code = append(code, OpLocalGet, 0) // get n
	code = append(code, OpCall, byte(printIntIdx))
	code = append(code, 0x1a) // drop result

	// Store newline at address 600
	code = append(code, 0x41)               // i32.const 600
	code = append(code, sleb128(600)...)
	code = append(code, 0x41, 0x0a)         // i32.const '\n'
	code = append(code, 0x3a, 0, 0)         // i32.store8

	// Set up iovec: buf=600, len=1
	code = append(code, 0x41, 0)            // i32.const 0 (iovec addr)
	code = append(code, 0x41)               // i32.const 600
	code = append(code, sleb128(600)...)
	code = append(code, OpI32Store, 2, 0)
	code = append(code, 0x41, 4)            // i32.const 4
	code = append(code, 0x41, 1)            // i32.const 1 (len)
	code = append(code, OpI32Store, 2, 0)

	// fd_write(1, 0, 1, 8)
	code = append(code, 0x41, 1) // fd
	code = append(code, 0x41, 0) // iovs
	code = append(code, 0x41, 1) // iovs_len
	code = append(code, 0x41, 8) // nwritten
	code = append(code, OpCall, byte(fdWriteIdx))

	return code
}

// generateAbsHelper generates the abs(n) helper function bytecode
// Returns the absolute value of n
func generateAbsHelper() []byte {
	var code []byte

	// if n >= 0, return n; else return -n
	code = append(code, OpLocalGet, 0)     // get n
	code = append(code, 0x41, 0)           // i32.const 0
	code = append(code, OpI32GeS)          // n >= 0
	code = append(code, OpIf, 0x7f)        // if (result i32)
	code = append(code, OpLocalGet, 0)     // return n
	code = append(code, OpElse)
	code = append(code, 0x41, 0)           // i32.const 0
	code = append(code, OpLocalGet, 0)     // get n
	code = append(code, OpI32Sub)          // 0 - n
	code = append(code, OpEnd)

	return code
}

// generateMinHelper generates the min(a, b) helper function bytecode
// Returns the smaller of a and b
func generateMinHelper() []byte {
	var code []byte

	// if a < b, return a; else return b
	code = append(code, OpLocalGet, 0)     // get a
	code = append(code, OpLocalGet, 1)     // get b
	code = append(code, OpI32LtS)          // a < b
	code = append(code, OpIf, 0x7f)        // if (result i32)
	code = append(code, OpLocalGet, 0)     // return a
	code = append(code, OpElse)
	code = append(code, OpLocalGet, 1)     // return b
	code = append(code, OpEnd)

	return code
}

// generateMaxHelper generates the max(a, b) helper function bytecode
// Returns the larger of a and b
func generateMaxHelper() []byte {
	var code []byte

	// if a > b, return a; else return b
	code = append(code, OpLocalGet, 0)     // get a
	code = append(code, OpLocalGet, 1)     // get b
	code = append(code, OpI32GtS)          // a > b
	code = append(code, OpIf, 0x7f)        // if (result i32)
	code = append(code, OpLocalGet, 0)     // return a
	code = append(code, OpElse)
	code = append(code, OpLocalGet, 1)     // return b
	code = append(code, OpEnd)

	return code
}

// generateSqrtHelper generates the sqrt(n) helper function bytecode
// Returns the integer square root of n using Newton's method
// Params: n (0)
// Locals: x (1), next (2)
func generateSqrtHelper() []byte {
	var code []byte

	// if n <= 1, return n (handles 0 and 1)
	code = append(code, OpLocalGet, 0)     // get n
	code = append(code, 0x41, 1)           // i32.const 1
	code = append(code, OpI32LeS)          // n <= 1
	code = append(code, OpIf, 0x7f)        // if (result i32)
	code = append(code, OpLocalGet, 0)     // return n
	code = append(code, OpElse)

	// x = n / 2 (initial guess)
	code = append(code, OpLocalGet, 0)     // get n
	code = append(code, 0x41, 2)           // i32.const 2
	code = append(code, OpI32Div)          // n / 2
	code = append(code, OpLocalSet, 1)     // local.set x

	// Newton's method loop (limited iterations for safety)
	// block
	code = append(code, OpBlock, 0x40)     // block (no result)
	// loop (8 iterations max)
	for i := 0; i < 8; i++ {
		// next = (x + n/x) / 2
		code = append(code, OpLocalGet, 1)     // get x
		code = append(code, OpLocalGet, 0)     // get n
		code = append(code, OpLocalGet, 1)     // get x
		code = append(code, OpI32Div)          // n / x
		code = append(code, OpI32Add)          // x + (n/x)
		code = append(code, 0x41, 2)           // i32.const 2
		code = append(code, OpI32Div)          // / 2
		code = append(code, OpLocalSet, 2)     // local.set next

		// if next >= x, break (converged)
		code = append(code, OpLocalGet, 2)     // get next
		code = append(code, OpLocalGet, 1)     // get x
		code = append(code, OpI32GeS)          // next >= x
		code = append(code, OpBrIf, 0)         // break if true

		// x = next
		code = append(code, OpLocalGet, 2)     // get next
		code = append(code, OpLocalSet, 1)     // local.set x
	}
	code = append(code, OpEnd)             // end block

	// return x
	code = append(code, OpLocalGet, 1)     // get x
	code = append(code, OpEnd)             // end if

	return code
}

// generateStrEqHelper generates the str_eq(addr1, len1, addr2, len2) helper function bytecode
// Returns 1 if strings are equal, 0 otherwise
// Params: addr1 (0), len1 (1), addr2 (2), len2 (3)
// Locals: i (4)
func generateStrEqHelper() []byte {
	var code []byte

	// if len1 != len2, return 0
	code = append(code, OpLocalGet, 1)     // len1
	code = append(code, OpLocalGet, 3)     // len2
	code = append(code, OpI32Ne)           // len1 != len2
	code = append(code, OpIf, 0x40)        // if (no result)
	code = append(code, 0x41, 0)           // i32.const 0
	code = append(code, 0x0f)              // return
	code = append(code, OpEnd)

	// i = 0 (local 4)
	code = append(code, 0x41, 0)           // i32.const 0
	code = append(code, OpLocalSet, 4)     // local.set i

	// loop: while i < len1
	code = append(code, OpBlock, 0x40)     // block (no result)
	code = append(code, OpLoop, 0x40)      // loop (no result)

	// if i >= len1, break
	code = append(code, OpLocalGet, 4)     // get i
	code = append(code, OpLocalGet, 1)     // get len1
	code = append(code, OpI32GeS)          // i >= len1
	code = append(code, OpBrIf, 1)         // break if true

	// load byte from addr1[i]
	code = append(code, OpLocalGet, 0)     // get addr1
	code = append(code, OpLocalGet, 4)     // get i
	code = append(code, OpI32Add)          // addr1 + i
	code = append(code, 0x2d, 0, 0)        // i32.load8_u (load unsigned byte)

	// load byte from addr2[i]
	code = append(code, OpLocalGet, 2)     // get addr2
	code = append(code, OpLocalGet, 4)     // get i
	code = append(code, OpI32Add)          // addr2 + i
	code = append(code, 0x2d, 0, 0)        // i32.load8_u

	// if bytes are not equal, return 0
	code = append(code, OpI32Ne)           // byte1 != byte2
	code = append(code, OpIf, 0x40)        // if (no result)
	code = append(code, 0x41, 0)           // i32.const 0
	code = append(code, 0x0f)              // return
	code = append(code, OpEnd)

	// i++
	code = append(code, OpLocalGet, 4)     // get i
	code = append(code, 0x41, 1)           // i32.const 1
	code = append(code, OpI32Add)          // i + 1
	code = append(code, OpLocalSet, 4)     // local.set i

	// continue loop
	code = append(code, OpBr, 0)           // br 0 (continue loop)
	code = append(code, OpEnd)             // end loop
	code = append(code, OpEnd)             // end block

	// All bytes matched, return 1
	code = append(code, 0x41, 1)           // i32.const 1

	return code
}

// generateStrCopyHelper generates the str_copy(src, len, dest) helper function bytecode
// Copies len bytes from src to dest, returns dest
// Params: src (0), len (1), dest (2)
// Locals: i (3)
func generateStrCopyHelper() []byte {
	var code []byte

	// i = 0 (local 3)
	code = append(code, 0x41, 0)           // i32.const 0
	code = append(code, OpLocalSet, 3)     // local.set i

	// loop: while i < len
	code = append(code, OpBlock, 0x40)     // block (no result)
	code = append(code, OpLoop, 0x40)      // loop (no result)

	// if i >= len, break
	code = append(code, OpLocalGet, 3)     // get i
	code = append(code, OpLocalGet, 1)     // get len
	code = append(code, OpI32GeS)          // i >= len
	code = append(code, OpBrIf, 1)         // break if true

	// dest[i] = src[i]
	code = append(code, OpLocalGet, 2)     // get dest
	code = append(code, OpLocalGet, 3)     // get i
	code = append(code, OpI32Add)          // dest + i

	code = append(code, OpLocalGet, 0)     // get src
	code = append(code, OpLocalGet, 3)     // get i
	code = append(code, OpI32Add)          // src + i
	code = append(code, 0x2d, 0, 0)        // i32.load8_u (load byte)

	code = append(code, 0x3a, 0, 0)        // i32.store8 (store byte)

	// i++
	code = append(code, OpLocalGet, 3)     // get i
	code = append(code, 0x41, 1)           // i32.const 1
	code = append(code, OpI32Add)          // i + 1
	code = append(code, OpLocalSet, 3)     // local.set i

	// continue loop
	code = append(code, OpBr, 0)           // br 0 (continue loop)
	code = append(code, OpEnd)             // end loop
	code = append(code, OpEnd)             // end block

	// Return dest
	code = append(code, OpLocalGet, 2)     // get dest

	return code
}

// generateMemcpyHelper generates the memcpy(dest, src, len) helper function bytecode
// Copies len bytes from src to dest, returns dest
// Params: dest (0), src (1), len (2)
// Locals: i (3)
func generateMemcpyHelper() []byte {
	var code []byte

	// i = 0 (local 3)
	code = append(code, 0x41, 0)           // i32.const 0
	code = append(code, OpLocalSet, 3)     // local.set i

	// loop: while i < len
	code = append(code, OpBlock, 0x40)     // block (no result)
	code = append(code, OpLoop, 0x40)      // loop (no result)

	// if i >= len, break
	code = append(code, OpLocalGet, 3)     // get i
	code = append(code, OpLocalGet, 2)     // get len
	code = append(code, OpI32GeS)          // i >= len
	code = append(code, OpBrIf, 1)         // break if true

	// dest[i] = src[i]
	code = append(code, OpLocalGet, 0)     // get dest
	code = append(code, OpLocalGet, 3)     // get i
	code = append(code, OpI32Add)          // dest + i

	code = append(code, OpLocalGet, 1)     // get src
	code = append(code, OpLocalGet, 3)     // get i
	code = append(code, OpI32Add)          // src + i
	code = append(code, 0x2d, 0, 0)        // i32.load8_u (load byte)

	code = append(code, 0x3a, 0, 0)        // i32.store8 (store byte)

	// i++
	code = append(code, OpLocalGet, 3)     // get i
	code = append(code, 0x41, 1)           // i32.const 1
	code = append(code, OpI32Add)          // i + 1
	code = append(code, OpLocalSet, 3)     // local.set i

	// continue loop
	code = append(code, OpBr, 0)           // br 0 (continue loop)
	code = append(code, OpEnd)             // end loop
	code = append(code, OpEnd)             // end block

	// Return dest
	code = append(code, OpLocalGet, 0)     // get dest

	return code
}

// generateReadCharHelper generates the read_char() helper function bytecode
// Returns the character read from stdin, or -1 on EOF/error
// Uses memory at address 700 for the 1-byte read buffer
// Uses memory at address 0 for iovec, address 8 for nread
func generateReadCharHelper(fdReadIdx int) []byte {
	var code []byte

	// Set up iovec at address 0:
	// iovec.buf = 16 (our read buffer, simple address)
	// iovec.buf_len = 1 (read 1 byte)
	code = append(code, 0x41, 0)           // i32.const 0 (iovec addr)
	code = append(code, 0x41, 16)          // i32.const 16
	code = append(code, OpI32Store, 2, 0)  // store buf address

	code = append(code, 0x41, 4)           // i32.const 4 (iovec.buf_len offset)
	code = append(code, 0x41, 1)           // i32.const 1 (read 1 byte)
	code = append(code, OpI32Store, 2, 0)  // store buf_len

	// fd_read(0, 0, 1, 8)
	// fd=0 (stdin), iovs=0, iovs_len=1, nread=8
	code = append(code, 0x41, 0)           // i32.const 0 (stdin)
	code = append(code, 0x41, 0)           // i32.const 0 (iovs)
	code = append(code, 0x41, 1)           // i32.const 1 (iovs_len)
	code = append(code, 0x41, 8)           // i32.const 8 (nread ptr)
	code = append(code, OpCall, byte(fdReadIdx))
	code = append(code, 0x1a)              // drop the result (errno)

	// Load the byte from address 16 and return it
	code = append(code, 0x41, 16)          // i32.const 16
	code = append(code, 0x2d, 0, 0)        // i32.load8_u (load unsigned byte)

	return code
}

// generateWriteCharHelper generates the write_char(c) helper function bytecode
// Writes a single character to stdout
// Params: c (i32) - the character to write
// Uses memory at address 800 for the 1-byte write buffer
// Uses memory at address 0 for iovec
func generateWriteCharHelper(fdWriteIdx int) []byte {
	var code []byte

	// Store the character at address 800
	code = append(code, 0x41, 0xa0, 0x06) // i32.const 800 (LEB128: 0xa0 0x06)
	code = append(code, OpLocalGet, 0)    // get character param
	code = append(code, 0x3a, 0, 0)       // i32.store8 (store byte)

	// Set up iovec at address 0:
	// iovec.buf = 800 (our write buffer)
	// iovec.buf_len = 1 (write 1 byte)
	code = append(code, 0x41, 0)           // i32.const 0 (iovec addr)
	code = append(code, 0x41, 0xa0, 0x06)  // i32.const 800 (LEB128)
	code = append(code, OpI32Store, 2, 0)  // store buf address

	code = append(code, 0x41, 4)           // i32.const 4 (iovec.buf_len offset)
	code = append(code, 0x41, 1)           // i32.const 1 (write 1 byte)
	code = append(code, OpI32Store, 2, 0)  // store buf_len

	// fd_write(1, 0, 1, 8)
	// fd=1 (stdout), iovs=0, iovs_len=1, nwritten=8
	code = append(code, 0x41, 1)           // i32.const 1 (stdout)
	code = append(code, 0x41, 0)           // i32.const 0 (iovs)
	code = append(code, 0x41, 1)           // i32.const 1 (iovs_len)
	code = append(code, 0x41, 8)           // i32.const 8 (nwritten ptr)
	code = append(code, OpCall, byte(fdWriteIdx))

	// Return the result (errno, 0 on success)
	return code
}

// generateMallocHelper generates the malloc(size) helper function bytecode
// Allocates size bytes on the heap and returns the address
// Params: size (0)
// Uses __heap_ptr global to track heap top
func generateMallocHelper(heapPtrIdx int) []byte {
	var code []byte

	// Get current heap pointer (this will be the return address)
	code = append(code, OpGlobalGet, byte(heapPtrIdx))

	// Add size to heap pointer: heap_ptr + size
	code = append(code, OpGlobalGet, byte(heapPtrIdx))
	code = append(code, OpLocalGet, 0)     // get size param
	code = append(code, OpI32Add)

	// Store new heap pointer
	code = append(code, OpGlobalSet, byte(heapPtrIdx))

	// Return the old heap pointer (which is still on stack)
	return code
}

// generatePrintIntHelper generates the _print_int helper function bytecode
// params: n (i32)
// locals: ptr, is_neg, digit
// Uses memory 500-520 as buffer, builds string backwards from 519
func generatePrintIntHelper(fdWriteIdx int) []byte {
	var code []byte

	// Locals: param n=0, ptr=1, is_neg=2, digit=3
	// Initialize ptr = 519 (end of buffer)
	code = append(code, 0x41) // i32.const 519
	code = append(code, sleb128(519)...)
	code = append(code, OpLocalSet, 1) // local.set 1 (ptr)

	// is_neg = 0
	code = append(code, 0x41, 0)       // i32.const 0
	code = append(code, OpLocalSet, 2) // local.set 2 (is_neg)

	// if n == 0, just store '0' and print
	code = append(code, OpLocalGet, 0) // local.get 0 (n)
	code = append(code, OpI32Eqz)      // i32.eqz
	code = append(code, OpIf, 0x40)    // if void
	// Store '0' at ptr
	code = append(code, OpLocalGet, 1) // ptr
	code = append(code, 0x41, 0x30)    // i32.const '0'
	code = append(code, 0x3a, 0, 0)    // i32.store8
	// Set length to 1 and jump to print
	code = append(code, 0x41, 0)       // i32.const 0 (iovec addr)
	code = append(code, OpLocalGet, 1) // ptr
	code = append(code, OpI32Store, 2, 0)
	code = append(code, 0x41, 4)    // i32.const 4
	code = append(code, 0x41, 1)    // i32.const 1 (len)
	code = append(code, OpI32Store, 2, 0)
	code = append(code, 0x41, 1)    // fd
	code = append(code, 0x41, 0)    // iovs
	code = append(code, 0x41, 1)    // iovs_len
	code = append(code, 0x41, 8)    // nwritten
	code = append(code, OpCall, byte(fdWriteIdx))
	code = append(code, 0x0f)       // return
	code = append(code, OpEnd)      // end if

	// if n < 0: is_neg = 1, n = -n
	code = append(code, OpLocalGet, 0) // n
	code = append(code, 0x41, 0)       // i32.const 0
	code = append(code, OpI32LtS)      // i32.lt_s
	code = append(code, OpIf, 0x40)    // if void
	code = append(code, 0x41, 1)       // i32.const 1
	code = append(code, OpLocalSet, 2) // is_neg = 1
	code = append(code, 0x41, 0)       // i32.const 0
	code = append(code, OpLocalGet, 0) // n
	code = append(code, OpI32Sub)      // 0 - n
	code = append(code, OpLocalSet, 0) // n = -n
	code = append(code, OpEnd)         // end if

	// Loop: extract digits
	code = append(code, OpBlock, 0x40) // block
	code = append(code, OpLoop, 0x40)  // loop
	// if n == 0, break
	code = append(code, OpLocalGet, 0) // n
	code = append(code, OpI32Eqz)
	code = append(code, OpBrIf, 1) // br_if to block (exit)
	// digit = n % 10
	code = append(code, OpLocalGet, 0) // n
	code = append(code, 0x41, 10)      // i32.const 10
	code = append(code, 0x6f)          // i32.rem_s
	code = append(code, OpLocalSet, 3) // digit
	// n = n / 10
	code = append(code, OpLocalGet, 0) // n
	code = append(code, 0x41, 10)      // i32.const 10
	code = append(code, OpI32Div)      // i32.div_s
	code = append(code, OpLocalSet, 0) // n = n / 10
	// store digit + '0' at ptr
	code = append(code, OpLocalGet, 1) // ptr
	code = append(code, OpLocalGet, 3) // digit
	code = append(code, 0x41, 0x30)    // i32.const '0'
	code = append(code, OpI32Add)
	code = append(code, 0x3a, 0, 0) // i32.store8
	// ptr--
	code = append(code, OpLocalGet, 1) // ptr
	code = append(code, 0x41, 1)       // i32.const 1
	code = append(code, OpI32Sub)
	code = append(code, OpLocalSet, 1) // ptr = ptr - 1
	// continue
	code = append(code, OpBr, 0)  // br to loop
	code = append(code, OpEnd)    // end loop
	code = append(code, OpEnd)    // end block

	// if is_neg, store '-' at ptr, ptr--
	code = append(code, OpLocalGet, 2) // is_neg
	code = append(code, OpIf, 0x40)
	code = append(code, OpLocalGet, 1) // ptr
	code = append(code, 0x41, 0x2d)    // i32.const '-'
	code = append(code, 0x3a, 0, 0)    // i32.store8
	code = append(code, OpLocalGet, 1)
	code = append(code, 0x41, 1)
	code = append(code, OpI32Sub)
	code = append(code, OpLocalSet, 1)
	code = append(code, OpEnd)

	// ptr now points one before the string start, so string starts at ptr+1
	// length = 519 - ptr
	// Set up iovec: buf = ptr+1, len = 519 - ptr
	code = append(code, 0x41, 0)       // i32.const 0 (iovec addr)
	code = append(code, OpLocalGet, 1) // ptr
	code = append(code, 0x41, 1)
	code = append(code, OpI32Add) // ptr + 1
	code = append(code, OpI32Store, 2, 0)

	code = append(code, 0x41, 4) // i32.const 4
	code = append(code, 0x41)
	code = append(code, sleb128(519)...) // i32.const 519
	code = append(code, OpLocalGet, 1)   // ptr
	code = append(code, OpI32Sub)        // 519 - ptr = length
	code = append(code, OpI32Store, 2, 0)

	// fd_write(1, 0, 1, 8)
	code = append(code, 0x41, 1) // fd
	code = append(code, 0x41, 0) // iovs
	code = append(code, 0x41, 1) // iovs_len
	code = append(code, 0x41, 8) // nwritten
	code = append(code, OpCall, byte(fdWriteIdx))

	return code
}

func CompileFile(file *parser.File, m *Module) {
	// Add internal heap pointer global for array allocation
	// Starts at 4096 to avoid conflicts with iovec (0-16), print buffer (500-600), and string table (1024+)
	m.AddGlobal("__heap_ptr", I32, true, 4096)
	heapPtrIdx := m.GlobalIndex("__heap_ptr")

	// Process user globals
	globalIdx := make(map[string]int)
	globalIdx["__heap_ptr"] = heapPtrIdx
	for _, g := range file.Globals {
		// For now, only support i32 globals with integer literal initializers
		var initVal int64
		if lit, ok := g.Value.(*parser.IntLit); ok {
			initVal = lit.Value
		}
		m.AddGlobal(g.Name, I32, true, initVal) // mutable i32
		globalIdx[g.Name] = m.GlobalIndex(g.Name)
	}

	// First pass: collect all function calls to detect WASI imports
	wasiImports := map[string]int{
		"fd_write":       4,
		"fd_read":        4,
		"args_get":       2,
		"args_sizes_get": 2,
		"proc_exit":      1,
	}

	usedImports := make(map[string]bool)
	for _, fn := range file.Fns {
		collectCalls(fn.Body, usedImports)
	}

	// Ensure fd_read is imported if read_char is used
	needsReadCharEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "read_char") {
			needsReadCharEarly = true
			break
		}
	}
	if needsReadCharEarly {
		usedImports["fd_read"] = true
	}

	// Ensure fd_write is imported if write_char is used
	needsWriteCharEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "write_char") {
			needsWriteCharEarly = true
			break
		}
	}
	if needsWriteCharEarly {
		usedImports["fd_write"] = true
	}

	// Add WASI imports first
	for name := range usedImports {
		if numParams, ok := wasiImports[name]; ok {
			m.AddImport("wasi_snapshot_preview1", name, numParams)
		}
	}

	// Build function index (imports are already indexed by AddImport)
	funcIdx := make(map[string]int)
	for name := range usedImports {
		if _, ok := wasiImports[name]; ok {
			funcIdx[name] = m.FuncIndex(name)
		}
	}
	for i, fn := range file.Fns {
		funcIdx[fn.Name] = len(m.imports) + i
	}

	// String table starts at offset 1024 (leave space for iovec, etc.)
	strings := NewStringTable(1024)

	// Check if print_int/println and math builtins are used
	needsPrintInt := false
	needsPrintln := false
	needsAbs := false
	needsMin := false
	needsMax := false
	needsSqrt := false
	needsStrEq := false
	needsStrCopy := false
	needsReadChar := false
	needsWriteChar := false
	needsMalloc := false
	needsMemcpy := false
	for _, fn := range file.Fns {
		if usesPrintInt(fn.Body) {
			needsPrintInt = true
		}
		if usesPrintln(fn.Body) {
			needsPrintln = true
		}
		if usesBuiltin(fn.Body, "abs") {
			needsAbs = true
		}
		if usesBuiltin(fn.Body, "min") {
			needsMin = true
		}
		if usesBuiltin(fn.Body, "max") {
			needsMax = true
		}
		if usesBuiltin(fn.Body, "sqrt") {
			needsSqrt = true
		}
		if usesBuiltin(fn.Body, "str_eq") {
			needsStrEq = true
		}
		if usesBuiltin(fn.Body, "str_copy") {
			needsStrCopy = true
		}
		if usesBuiltin(fn.Body, "read_char") {
			needsReadChar = true
		}
		if usesBuiltin(fn.Body, "write_char") {
			needsWriteChar = true
		}
		if usesBuiltin(fn.Body, "malloc") {
			needsMalloc = true
		}
		if usesBuiltin(fn.Body, "memcpy") {
			needsMemcpy = true
		}
	}

	// Count helper functions needed
	helperCount := 0
	if needsPrintInt {
		helperCount++
	}
	if needsPrintln {
		helperCount++
	}
	if needsAbs {
		helperCount++
	}
	if needsMin {
		helperCount++
	}
	if needsMax {
		helperCount++
	}
	if needsSqrt {
		helperCount++
	}
	if needsStrEq {
		helperCount++
	}
	if needsStrCopy {
		helperCount++
	}
	if needsReadChar {
		helperCount++
	}
	if needsWriteChar {
		helperCount++
	}
	if needsMalloc {
		helperCount++
	}
	if needsMemcpy {
		helperCount++
	}

	// Adjust function indices for helpers
	helperIdx := 0
	if needsPrintInt {
		funcIdx["_print_int"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsPrintln {
		funcIdx["_println"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsAbs {
		funcIdx["abs"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsMin {
		funcIdx["min"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsMax {
		funcIdx["max"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsSqrt {
		funcIdx["sqrt"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsStrEq {
		funcIdx["str_eq"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsStrCopy {
		funcIdx["str_copy"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsReadChar {
		funcIdx["read_char"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsWriteChar {
		funcIdx["write_char"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsMalloc {
		funcIdx["malloc"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsMemcpy {
		funcIdx["memcpy"] = len(m.imports) + helperIdx
		helperIdx++
	}
	for i, fn := range file.Fns {
		funcIdx[fn.Name] = len(m.imports) + helperCount + i
	}

	// Add helper functions
	if needsPrintInt {
		code := generatePrintIntHelper(funcIdx["fd_write"])
		m.AddFunction("_print_int", 1, code, 3) // 1 param, 3 locals
	}
	if needsPrintln {
		code := generatePrintlnHelper(funcIdx["_print_int"], funcIdx["fd_write"])
		m.AddFunction("_println", 1, code, 0) // 1 param, 0 locals
	}
	if needsAbs {
		code := generateAbsHelper()
		m.AddFunction("abs", 1, code, 0) // 1 param, 0 locals
	}
	if needsMin {
		code := generateMinHelper()
		m.AddFunction("min", 2, code, 0) // 2 params, 0 locals
	}
	if needsMax {
		code := generateMaxHelper()
		m.AddFunction("max", 2, code, 0) // 2 params, 0 locals
	}
	if needsSqrt {
		code := generateSqrtHelper()
		m.AddFunction("sqrt", 1, code, 2) // 1 param, 2 locals (x, next)
	}
	if needsStrEq {
		code := generateStrEqHelper()
		m.AddFunction("str_eq", 4, code, 1) // 4 params, 1 local
	}
	if needsStrCopy {
		code := generateStrCopyHelper()
		m.AddFunction("str_copy", 3, code, 1) // 3 params, 1 local
	}
	if needsReadChar {
		code := generateReadCharHelper(funcIdx["fd_read"])
		m.AddFunction("read_char", 0, code, 0) // 0 params, 0 locals
	}
	if needsWriteChar {
		code := generateWriteCharHelper(funcIdx["fd_write"])
		m.AddFunction("write_char", 1, code, 0) // 1 param (char), 0 locals
	}
	if needsMalloc {
		code := generateMallocHelper(heapPtrIdx)
		m.AddFunction("malloc", 1, code, 0) // 1 param (size), 0 locals
	}
	if needsMemcpy {
		code := generateMemcpyHelper()
		m.AddFunction("memcpy", 3, code, 1) // 3 params (dest, src, len), 1 local
	}

	for _, fn := range file.Fns {
		code, numLocals := Compile(fn, funcIdx, globalIdx, strings)
		m.AddFunction(fn.Name, len(fn.Params), code, numLocals)
	}

	// Add string data to module
	if len(strings.Bytes()) > 0 {
		m.AddData(strings.offset, strings.Bytes())
	}
}

func usesPrintInt(block *parser.Block) bool {
	for _, stmt := range block.Stmts {
		if usesPrintIntStmt(stmt) {
			return true
		}
	}
	if block.Expr != nil && usesPrintIntExpr(block.Expr) {
		return true
	}
	return false
}

func usesPrintIntStmt(stmt parser.Stmt) bool {
	switch s := stmt.(type) {
	case *parser.LetStmt:
		return usesPrintIntExpr(s.Value)
	case *parser.AssignStmt:
		return usesPrintIntExpr(s.Value)
	case *parser.ExprStmt:
		return usesPrintIntExpr(s.Expr)
	}
	return false
}

func usesPrintIntExpr(expr parser.Expr) bool {
	switch e := expr.(type) {
	case *parser.CallExpr:
		if e.Name == "print_int" || e.Name == "println" {
			return true
		}
		for _, arg := range e.Args {
			if usesPrintIntExpr(arg) {
				return true
			}
		}
	case *parser.BinaryExpr:
		return usesPrintIntExpr(e.Left) || usesPrintIntExpr(e.Right)
	case *parser.IfExpr:
		if usesPrintIntExpr(e.Cond) || usesPrintInt(e.Then) {
			return true
		}
		if e.Else != nil && usesPrintInt(e.Else) {
			return true
		}
	case *parser.LoopExpr:
		return usesPrintIntExpr(e.Cond) || usesPrintInt(e.Body)
	case *parser.Block:
		return usesPrintInt(e)
	}
	return false
}

func usesPrintln(block *parser.Block) bool {
	for _, stmt := range block.Stmts {
		if usesPrintlnStmt(stmt) {
			return true
		}
	}
	if block.Expr != nil && usesPrintlnExpr(block.Expr) {
		return true
	}
	return false
}

func usesPrintlnStmt(stmt parser.Stmt) bool {
	switch s := stmt.(type) {
	case *parser.LetStmt:
		return usesPrintlnExpr(s.Value)
	case *parser.AssignStmt:
		return usesPrintlnExpr(s.Value)
	case *parser.ExprStmt:
		return usesPrintlnExpr(s.Expr)
	}
	return false
}

func usesPrintlnExpr(expr parser.Expr) bool {
	switch e := expr.(type) {
	case *parser.CallExpr:
		if e.Name == "println" {
			return true
		}
		for _, arg := range e.Args {
			if usesPrintlnExpr(arg) {
				return true
			}
		}
	case *parser.BinaryExpr:
		return usesPrintlnExpr(e.Left) || usesPrintlnExpr(e.Right)
	case *parser.IfExpr:
		if usesPrintlnExpr(e.Cond) || usesPrintln(e.Then) {
			return true
		}
		if e.Else != nil && usesPrintln(e.Else) {
			return true
		}
	case *parser.LoopExpr:
		return usesPrintlnExpr(e.Cond) || usesPrintln(e.Body)
	case *parser.Block:
		return usesPrintln(e)
	}
	return false
}

func usesBuiltin(block *parser.Block, name string) bool {
	for _, stmt := range block.Stmts {
		if usesBuiltinStmt(stmt, name) {
			return true
		}
	}
	if block.Expr != nil && usesBuiltinExpr(block.Expr, name) {
		return true
	}
	return false
}

func usesBuiltinStmt(stmt parser.Stmt, name string) bool {
	switch s := stmt.(type) {
	case *parser.LetStmt:
		return usesBuiltinExpr(s.Value, name)
	case *parser.AssignStmt:
		return usesBuiltinExpr(s.Value, name)
	case *parser.ExprStmt:
		return usesBuiltinExpr(s.Expr, name)
	case *parser.IndexAssignStmt:
		return usesBuiltinExpr(s.Array, name) || usesBuiltinExpr(s.Index, name) || usesBuiltinExpr(s.Value, name)
	}
	return false
}

func usesBuiltinExpr(expr parser.Expr, name string) bool {
	switch e := expr.(type) {
	case *parser.CallExpr:
		if e.Name == name {
			return true
		}
		for _, arg := range e.Args {
			if usesBuiltinExpr(arg, name) {
				return true
			}
		}
	case *parser.BinaryExpr:
		return usesBuiltinExpr(e.Left, name) || usesBuiltinExpr(e.Right, name)
	case *parser.UnaryExpr:
		return usesBuiltinExpr(e.Expr, name)
	case *parser.IfExpr:
		if usesBuiltinExpr(e.Cond, name) || usesBuiltin(e.Then, name) {
			return true
		}
		if e.Else != nil && usesBuiltin(e.Else, name) {
			return true
		}
	case *parser.LoopExpr:
		return usesBuiltinExpr(e.Cond, name) || usesBuiltin(e.Body, name)
	case *parser.Block:
		return usesBuiltin(e, name)
	case *parser.ReturnExpr:
		return usesBuiltinExpr(e.Value, name)
	case *parser.IndexExpr:
		return usesBuiltinExpr(e.Array, name) || usesBuiltinExpr(e.Index, name)
	case *parser.ArrayLit:
		for _, elem := range e.Elements {
			if usesBuiltinExpr(elem, name) {
				return true
			}
		}
	}
	return false
}

func collectCalls(block *parser.Block, calls map[string]bool) {
	for _, stmt := range block.Stmts {
		collectCallsStmt(stmt, calls)
	}
	if block.Expr != nil {
		collectCallsExpr(block.Expr, calls)
	}
}

func collectCallsStmt(stmt parser.Stmt, calls map[string]bool) {
	switch s := stmt.(type) {
	case *parser.LetStmt:
		collectCallsExpr(s.Value, calls)
	case *parser.AssignStmt:
		collectCallsExpr(s.Value, calls)
	case *parser.ExprStmt:
		collectCallsExpr(s.Expr, calls)
	}
}

func collectCallsExpr(expr parser.Expr, calls map[string]bool) {
	switch e := expr.(type) {
	case *parser.CallExpr:
		if e.Name == "print_str" || e.Name == "print_int" || e.Name == "println" || e.Name == "print" {
			calls["fd_write"] = true
		} else if e.Name == "exit" {
			calls["proc_exit"] = true
		} else {
			calls[e.Name] = true
		}
		for _, arg := range e.Args {
			collectCallsExpr(arg, calls)
		}
	case *parser.BinaryExpr:
		collectCallsExpr(e.Left, calls)
		collectCallsExpr(e.Right, calls)
	case *parser.IfExpr:
		collectCallsExpr(e.Cond, calls)
		collectCalls(e.Then, calls)
		if e.Else != nil {
			collectCalls(e.Else, calls)
		}
	case *parser.LoopExpr:
		collectCallsExpr(e.Cond, calls)
		collectCalls(e.Body, calls)
	case *parser.Block:
		collectCalls(e, calls)
	}
}

func Compile(fn *parser.FnDecl, funcIdx, globalIdx map[string]int, strings *StringTable) (code []byte, numLocals int) {
	c := &Compiler{
		fn:        fn,
		locals:    make(map[string]int),
		funcIdx:   funcIdx,
		globalIdx: globalIdx,
		strings:   strings,
	}

	// Map params to local indices
	for i, p := range fn.Params {
		c.locals[p.Name] = i
	}
	c.numLocals = len(fn.Params)

	// First pass: count locals from let statements
	for _, stmt := range fn.Body.Stmts {
		if let, ok := stmt.(*parser.LetStmt); ok {
			c.locals[let.Name] = c.numLocals
			c.numLocals++
		}
	}

	code = c.compileBlock(fn.Body)
	return code, c.numLocals - len(fn.Params)
}

func (c *Compiler) compileBlock(b *parser.Block) []byte {
	var code []byte
	for _, stmt := range b.Stmts {
		code = append(code, c.compileStmt(stmt)...)
	}
	if b.Expr != nil {
		code = append(code, c.compileExpr(b.Expr)...)
	}
	return code
}

func (c *Compiler) compileStmt(s parser.Stmt) []byte {
	switch s := s.(type) {
	case *parser.LetStmt:
		code := c.compileExpr(s.Value)
		idx := c.locals[s.Name]
		code = append(code, OpLocalSet, byte(idx))
		return code
	case *parser.AssignStmt:
		code := c.compileExpr(s.Value)
		// Check if it's a global variable
		if idx, ok := c.globalIdx[s.Name]; ok {
			code = append(code, OpGlobalSet, byte(idx))
			return code
		}
		idx := c.locals[s.Name]
		code = append(code, OpLocalSet, byte(idx))
		return code
	case *parser.IndexAssignStmt:
		// Store value at array[index]: compute addr, then store
		var code []byte
		// Compute address: array + index * 4
		code = append(code, c.compileExpr(s.Array)...)
		code = append(code, c.compileExpr(s.Index)...)
		code = append(code, 0x41, 4) // i32.const 4
		code = append(code, OpI32Mul)
		code = append(code, OpI32Add)
		// Store the value
		code = append(code, c.compileExpr(s.Value)...)
		code = append(code, OpI32Store, 2, 0)
		return code
	case *parser.ExprStmt:
		code := c.compileExpr(s.Expr)
		if exprProducesValue(s.Expr) {
			code = append(code, 0x1a) // drop
		}
		return code
	}
	return nil
}

func exprProducesValue(e parser.Expr) bool {
	switch e := e.(type) {
	case *parser.LoopExpr:
		return false
	case *parser.IfExpr:
		return e.Else != nil // if-without-else produces void
	case *parser.BreakExpr, *parser.ContinueExpr, *parser.ReturnExpr:
		return false
	case *parser.CallExpr:
		return e.Name != "drop" && e.Name != "store"
	default:
		return true
	}
}

func (c *Compiler) compileExpr(e parser.Expr) []byte {
	switch e := e.(type) {
	case *parser.IntLit:
		return append([]byte{0x41}, sleb128(e.Value)...) // i32.const
	case *parser.StringLit:
		addr := c.strings.Add(e.Value)
		return append([]byte{0x41}, sleb128(int64(addr))...) // i32.const addr
	case *parser.Ident:
		// Check if it's a global variable first
		if idx, ok := c.globalIdx[e.Name]; ok {
			return []byte{OpGlobalGet, byte(idx)}
		}
		idx := c.locals[e.Name]
		return []byte{OpLocalGet, byte(idx)}
	case *parser.UnaryExpr:
		var code []byte
		switch e.Op {
		case "!":
			code = append(code, c.compileExpr(e.Expr)...)
			code = append(code, OpI32Eqz) // !x is x == 0
		case "-":
			code = append(code, 0x41, 0) // i32.const 0
			code = append(code, c.compileExpr(e.Expr)...)
			code = append(code, OpI32Sub) // 0 - x
		}
		return code
	case *parser.BinaryExpr:
		var code []byte
		code = append(code, c.compileExpr(e.Left)...)
		code = append(code, c.compileExpr(e.Right)...)
		switch e.Op {
		case "+":
			code = append(code, OpI32Add)
		case "-":
			code = append(code, OpI32Sub)
		case "*":
			code = append(code, OpI32Mul)
		case "/":
			code = append(code, OpI32Div)
		case "%":
			code = append(code, 0x6f) // i32.rem_s
		case "==":
			code = append(code, OpI32Eq)
		case "!=":
			code = append(code, OpI32Ne)
		case "<":
			code = append(code, OpI32LtS)
		case ">":
			code = append(code, OpI32GtS)
		case "<=":
			code = append(code, OpI32LeS)
		case ">=":
			code = append(code, OpI32GeS)
		case "&&":
			code = append(code, 0x71) // i32.and
		case "||":
			code = append(code, 0x72) // i32.or
		case "&":
			code = append(code, 0x71) // i32.and
		case "|":
			code = append(code, 0x72) // i32.or
		case "^":
			code = append(code, 0x73) // i32.xor
		case "<<":
			code = append(code, 0x74) // i32.shl
		case ">>":
			code = append(code, 0x75) // i32.shr_s (signed)
		}
		return code
	case *parser.IfExpr:
		var code []byte
		code = append(code, c.compileExpr(e.Cond)...)
		// Entering if block increases depth for break/continue labels
		c.breakLabel++
		c.continueLabel++
		if e.Else != nil {
			// if-else: produces a value
			code = append(code, OpIf, 0x7f) // if with i32 result
			code = append(code, c.compileBlock(e.Then)...)
			code = append(code, OpElse)
			code = append(code, c.compileBlock(e.Else)...)
		} else {
			// if-without-else: void block (used for side effects)
			code = append(code, OpIf, 0x40) // if with void result
			code = append(code, c.compileBlock(e.Then)...)
		}
		c.breakLabel--
		c.continueLabel--
		code = append(code, OpEnd)
		return code
	case *parser.LoopExpr:
		// block $exit
		//   loop $continue
		//     br_if $exit (i32.eqz condition)
		//     body
		//     br $continue
		//   end
		// end
		var code []byte
		code = append(code, OpBlock, 0x40) // block with void result
		code = append(code, OpLoop, 0x40)  // loop with void result
		code = append(code, c.compileExpr(e.Cond)...)
		code = append(code, OpI32Eqz)      // invert condition
		code = append(code, OpBrIf, 1)     // br_if to block (exit)
		// Save old labels and set new ones (loop=0, block=1 from inside loop body)
		oldBreak, oldContinue := c.breakLabel, c.continueLabel
		c.breakLabel = 1   // outer block
		c.continueLabel = 0 // loop
		c.loopDepth++
		code = append(code, c.compileBlock(e.Body)...)
		c.loopDepth--
		c.breakLabel, c.continueLabel = oldBreak, oldContinue
		code = append(code, OpBr, 0)       // br to loop (continue)
		code = append(code, OpEnd)         // end loop
		code = append(code, OpEnd)         // end block
		return code
	case *parser.BreakExpr:
		// br to outer block (exit loop)
		return []byte{OpBr, byte(c.breakLabel)}
	case *parser.ContinueExpr:
		// br to loop start
		return []byte{OpBr, byte(c.continueLabel)}
	case *parser.ReturnExpr:
		// return value
		var code []byte
		code = append(code, c.compileExpr(e.Value)...)
		code = append(code, 0x0f) // return
		return code
	case *parser.Block:
		return c.compileBlock(e)
	case *parser.ArrayLit:
		// Allocate space on heap and store elements
		// Returns the base address of the array
		var code []byte
		heapIdx := c.globalIdx["__heap_ptr"]

		// Save current heap pointer as the array base address
		code = append(code, OpGlobalGet, byte(heapIdx))

		// For each element, store it at heap_ptr + i*4
		for i, elem := range e.Elements {
			// Compute address: heap_ptr + i*4
			code = append(code, OpGlobalGet, byte(heapIdx))
			code = append(code, 0x41) // i32.const
			code = append(code, sleb128(int64(i*4))...)
			code = append(code, OpI32Add)
			// Store the element value
			code = append(code, c.compileExpr(elem)...)
			code = append(code, OpI32Store, 2, 0)
		}

		// Update heap pointer: heap_ptr += len * 4
		code = append(code, OpGlobalGet, byte(heapIdx))
		code = append(code, 0x41) // i32.const
		code = append(code, sleb128(int64(len(e.Elements)*4))...)
		code = append(code, OpI32Add)
		code = append(code, OpGlobalSet, byte(heapIdx))

		// The base address is already on the stack from the first global.get
		return code
	case *parser.IndexExpr:
		// Load value at base + index * 4
		var code []byte
		code = append(code, c.compileExpr(e.Array)...)
		code = append(code, c.compileExpr(e.Index)...)
		code = append(code, 0x41, 4) // i32.const 4
		code = append(code, OpI32Mul)
		code = append(code, OpI32Add)
		code = append(code, OpI32Load, 2, 0)
		return code
	case *parser.CallExpr:
		var code []byte
		for _, arg := range e.Args {
			code = append(code, c.compileExpr(arg)...)
		}
		switch e.Name {
		case "load":
			code = append(code, OpI32Load, 2, 0) // align=4, offset=0
		case "store":
			code = append(code, OpI32Store, 2, 0)
		case "drop":
			code = append(code, 0x1a) // drop opcode
		case "print_str":
			// Args on stack: addr, len
			// Set up iovec at addr 0: store addr at 0, len at 4
			// Then call fd_write(1, 0, 1, 8)
			code = nil // clear, we'll handle this specially
			addr := c.compileExpr(e.Args[0])
			length := c.compileExpr(e.Args[1])
			// store addr at 0
			code = append(code, 0x41, 0) // i32.const 0
			code = append(code, addr...)
			code = append(code, OpI32Store, 2, 0)
			// store len at 4
			code = append(code, 0x41, 4) // i32.const 4
			code = append(code, length...)
			code = append(code, OpI32Store, 2, 0)
			// fd_write(1, 0, 1, 8)
			code = append(code, 0x41, 1) // fd = 1 (stdout)
			code = append(code, 0x41, 0) // iovs = 0
			code = append(code, 0x41, 1) // iovs_len = 1
			code = append(code, 0x41, 8) // nwritten = 8
			idx := c.funcIdx["fd_write"]
			code = append(code, OpCall, byte(idx))
		case "exit":
			// proc_exit(code) - doesn't return
			idx := c.funcIdx["proc_exit"]
			code = append(code, OpCall, byte(idx))
			code = append(code, 0x00) // unreachable
		case "print_int":
			// Call the _print_int helper function
			idx := c.funcIdx["_print_int"]
			code = append(code, OpCall, byte(idx))
		case "println":
			// Call the _println helper function
			idx := c.funcIdx["_println"]
			code = append(code, OpCall, byte(idx))
		case "abs":
			// abs(n) - returns absolute value
			// if n < 0 { -n } else { n }
			code = append(code, OpLocalGet, 0) // duplicate for comparison
			code = nil
			code = append(code, c.compileExpr(e.Args[0])...)
			code = append(code, 0x41, 0) // i32.const 0
			code = append(code, OpI32LtS)
			code = append(code, OpIf, 0x7f) // if with i32 result
			code = append(code, 0x41, 0)
			code = append(code, c.compileExpr(e.Args[0])...)
			code = append(code, OpI32Sub) // 0 - n
			code = append(code, OpElse)
			code = append(code, c.compileExpr(e.Args[0])...)
			code = append(code, OpEnd)
		case "min":
			// min(a, b) - returns smaller value
			code = nil
			a := c.compileExpr(e.Args[0])
			b := c.compileExpr(e.Args[1])
			code = append(code, a...)
			code = append(code, b...)
			code = append(code, OpI32LtS)
			code = append(code, OpIf, 0x7f)
			code = append(code, a...)
			code = append(code, OpElse)
			code = append(code, b...)
			code = append(code, OpEnd)
		case "max":
			// max(a, b) - returns larger value
			code = nil
			a := c.compileExpr(e.Args[0])
			b := c.compileExpr(e.Args[1])
			code = append(code, a...)
			code = append(code, b...)
			code = append(code, OpI32GtS)
			code = append(code, OpIf, 0x7f)
			code = append(code, a...)
			code = append(code, OpElse)
			code = append(code, b...)
			code = append(code, OpEnd)
		case "print":
			// print(str, len) - prints string with newline
			code = nil
			addr := c.compileExpr(e.Args[0])
			length := c.compileExpr(e.Args[1])
			// store addr at 0
			code = append(code, 0x41, 0)
			code = append(code, addr...)
			code = append(code, OpI32Store, 2, 0)
			// store len at 4
			code = append(code, 0x41, 4)
			code = append(code, length...)
			code = append(code, OpI32Store, 2, 0)
			// fd_write(1, 0, 1, 8)
			code = append(code, 0x41, 1)
			code = append(code, 0x41, 0)
			code = append(code, 0x41, 1)
			code = append(code, 0x41, 8)
			idx := c.funcIdx["fd_write"]
			code = append(code, OpCall, byte(idx))
			code = append(code, 0x1a) // drop result
			// Print newline: store '\n' at 600, iovec at 0
			code = append(code, 0x41)
			code = append(code, sleb128(600)...)
			code = append(code, 0x41, 0x0a)
			code = append(code, 0x3a, 0, 0) // i32.store8
			code = append(code, 0x41, 0)
			code = append(code, 0x41)
			code = append(code, sleb128(600)...)
			code = append(code, OpI32Store, 2, 0)
			code = append(code, 0x41, 4)
			code = append(code, 0x41, 1)
			code = append(code, OpI32Store, 2, 0)
			code = append(code, 0x41, 1)
			code = append(code, 0x41, 0)
			code = append(code, 0x41, 1)
			code = append(code, 0x41, 8)
			code = append(code, OpCall, byte(idx))
		default:
			idx := c.funcIdx[e.Name]
			code = append(code, OpCall, byte(idx))
		}
		return code
	}
	return nil
}
