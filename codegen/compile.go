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
	OpReturn    = 0x0f
	OpCall      = 0x10
	OpDrop      = 0x1a
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
	OpI32RemS   = 0x6f
	OpI32And    = 0x71
	OpI32Or     = 0x72
	OpI32Xor    = 0x73
	OpI32Shl    = 0x74
	OpI32ShrS   = 0x75
	OpI32ShrU   = 0x76
	OpI32Rotl   = 0x77
	OpI32Rotr   = 0x78
	OpI32Clz    = 0x67
	OpI32Ctz    = 0x68
	OpI32Popcnt = 0x69

	OpI32Load  = 0x28
	OpI64Load  = 0x29
	OpI32Store = 0x36
)

type Compiler struct {
	fn            *parser.FnDecl
	locals        map[string]int
	localStructs  map[string]string // maps local variable name to struct type name
	numLocals     int
	funcIdx       map[string]int
	globalIdx     map[string]int
	structs       map[string]*StructInfo
	strings       *StringTable
	loopDepth     int // depth of nested loops
	breakLabel    int // label offset for break (to outer block)
	continueLabel int // label offset for continue (to loop)
	isAsync       bool // true if compiling an async function
	continuation  string // current continuation label for async functions
	asyncRuntime  bool // true if async runtime support is needed
}

// StructInfo contains information about a struct type
type StructInfo struct {
	Name    string
	Fields  []StructFieldInfo
	Size    int // total size in bytes
	Offsets map[string]int // field name -> byte offset
}

// StructFieldInfo contains information about a struct field
type StructFieldInfo struct {
	Name   string
	Type   string
	Size   int
	Offset int
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

// generateModHelper generates the mod(a, b) helper function bytecode
// Returns a % b (signed remainder)
// Params: a (0), b (1)
func generateModHelper() []byte {
	var code []byte

	// return a % b
	code = append(code, OpLocalGet, 0)     // get a
	code = append(code, OpLocalGet, 1)     // get b
	code = append(code, OpI32RemS)         // a % b

	return code
}

// generateAndHelper generates the and(a, b) helper function bytecode
// Returns a & b (bitwise AND)
// Params: a (0), b (1)
func generateAndHelper() []byte {
	var code []byte

	code = append(code, OpLocalGet, 0)     // get a
	code = append(code, OpLocalGet, 1)     // get b
	code = append(code, OpI32And)          // a & b

	return code
}

// generateOrHelper generates the or(a, b) helper function bytecode
// Returns a | b (bitwise OR)
// Params: a (0), b (1)
func generateOrHelper() []byte {
	var code []byte

	code = append(code, OpLocalGet, 0)     // get a
	code = append(code, OpLocalGet, 1)     // get b
	code = append(code, OpI32Or)           // a | b

	return code
}

// generateXorHelper generates the xor(a, b) helper function bytecode
// Returns a ^ b (bitwise XOR)
// Params: a (0), b (1)
func generateXorHelper() []byte {
	var code []byte

	code = append(code, OpLocalGet, 0)     // get a
	code = append(code, OpLocalGet, 1)     // get b
	code = append(code, OpI32Xor)          // a ^ b

	return code
}

// generateShlHelper generates the shl(a, b) helper function bytecode
// Returns a << b (shift left)
// Params: a (0), b (1)
func generateShlHelper() []byte {
	var code []byte

	code = append(code, OpLocalGet, 0)     // get a
	code = append(code, OpLocalGet, 1)     // get b
	code = append(code, OpI32Shl)          // a << b

	return code
}

// generateShrHelper generates the shr(a, b) helper function bytecode
// Returns a >> b (shift right signed)
// Params: a (0), b (1)
func generateShrHelper() []byte {
	var code []byte

	code = append(code, OpLocalGet, 0)     // get a
	code = append(code, OpLocalGet, 1)     // get b
	code = append(code, OpI32ShrS)         // a >> b (signed)

	return code
}

// generateClzHelper generates the clz(n) helper function bytecode
// Returns count of leading zero bits
// Params: n (0)
func generateClzHelper() []byte {
	var code []byte

	code = append(code, OpLocalGet, 0)     // get n
	code = append(code, OpI32Clz)          // i32.clz

	return code
}

// generateCtzHelper generates the ctz(n) helper function bytecode
// Returns count of trailing zero bits
// Params: n (0)
func generateCtzHelper() []byte {
	var code []byte

	code = append(code, OpLocalGet, 0)     // get n
	code = append(code, OpI32Ctz)          // i32.ctz

	return code
}

// generatePopcntHelper generates the popcnt(n) helper function bytecode
// Returns count of 1 bits (population count)
// Params: n (0)
func generatePopcntHelper() []byte {
	var code []byte

	code = append(code, OpLocalGet, 0)     // get n
	code = append(code, OpI32Popcnt)       // i32.popcnt

	return code
}

// generateRotlHelper generates the rotl(n, k) helper function bytecode
// Returns n rotated left by k bits
// Params: n (0), k (1)
func generateRotlHelper() []byte {
	var code []byte

	code = append(code, OpLocalGet, 0)     // get n
	code = append(code, OpLocalGet, 1)     // get k
	code = append(code, OpI32Rotl)         // i32.rotl

	return code
}

// generateRotrHelper generates the rotr(n, k) helper function bytecode
// Returns n rotated right by k bits
// Params: n (0), k (1)
func generateRotrHelper() []byte {
	var code []byte

	code = append(code, OpLocalGet, 0)     // get n
	code = append(code, OpLocalGet, 1)     // get k
	code = append(code, OpI32Rotr)         // i32.rotr

	return code
}

// generatePowHelper generates the pow(base, exp) helper function bytecode
// Returns base^exp for non-negative integer exponent
// Params: base (0), exp (1)
// Locals: result (2), i (3)
func generatePowHelper() []byte {
	var code []byte

	// if exp == 0, return 1
	code = append(code, OpLocalGet, 1)     // get exp
	code = append(code, OpI32Eqz)          // exp == 0
	code = append(code, OpIf, 0x7f)        // if (result i32)
	code = append(code, 0x41, 1)           // return 1
	code = append(code, OpElse)

	// if exp == 1, return base
	code = append(code, OpLocalGet, 1)     // get exp
	code = append(code, 0x41, 1)           // i32.const 1
	code = append(code, OpI32Eq)           // exp == 1
	code = append(code, OpIf, 0x7f)        // if (result i32)
	code = append(code, OpLocalGet, 0)     // return base
	code = append(code, OpElse)

	// result = 1
	code = append(code, 0x41, 1)           // i32.const 1
	code = append(code, OpLocalSet, 2)     // local.set result

	// i = 0
	code = append(code, 0x41, 0)           // i32.const 0
	code = append(code, OpLocalSet, 3)     // local.set i

	// loop: while i < exp
	code = append(code, OpBlock, 0x40)     // block (no result)
	code = append(code, OpLoop, 0x40)      // loop (no result)

	// if i >= exp, break
	code = append(code, OpLocalGet, 3)     // get i
	code = append(code, OpLocalGet, 1)     // get exp
	code = append(code, OpI32GeS)          // i >= exp
	code = append(code, OpBrIf, 1)         // break if true

	// result = result * base
	code = append(code, OpLocalGet, 2)     // get result
	code = append(code, OpLocalGet, 0)     // get base
	code = append(code, OpI32Mul)          // result * base
	code = append(code, OpLocalSet, 2)     // local.set result

	// i++
	code = append(code, OpLocalGet, 3)     // get i
	code = append(code, 0x41, 1)           // i32.const 1
	code = append(code, OpI32Add)          // i + 1
	code = append(code, OpLocalSet, 3)     // local.set i

	// continue loop
	code = append(code, OpBr, 0)           // br 0 (continue loop)
	code = append(code, OpEnd)             // end loop
	code = append(code, OpEnd)             // end block

	// return result
	code = append(code, OpLocalGet, 2)     // get result
	code = append(code, OpEnd)             // end inner if
	code = append(code, OpEnd)             // end outer if

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

// generateRandomHelper generates the random() helper function bytecode
// Returns a random i32 value using WASI random_get
// Uses memory at address 900 for the 4-byte random buffer
func generateRandomHelper(randomGetIdx int) []byte {
	var code []byte

	// random_get(900, 4) - get 4 random bytes at address 900
	code = append(code, 0x41, 0x84, 0x07) // i32.const 900 (LEB128)
	code = append(code, 0x41, 4)          // i32.const 4 (length)
	code = append(code, OpCall, byte(randomGetIdx))
	code = append(code, 0x1a)             // drop result (errno)

	// Load the 4 bytes as an i32
	code = append(code, 0x41, 0x84, 0x07) // i32.const 900
	code = append(code, OpI32Load, 2, 0)  // i32.load

	return code
}

// generateExitHelper generates the exit(code) helper function bytecode
// Exits the process with the given exit code using WASI proc_exit
// Params: code (i32) - the exit code
func generateExitHelper(procExitIdx int) []byte {
	var code []byte

	// proc_exit(code)
	code = append(code, OpLocalGet, 0)    // get exit code param
	code = append(code, OpCall, byte(procExitIdx))

	// proc_exit never returns, but WASM requires a return value
	// Return 0 as a placeholder (unreachable)
	code = append(code, 0x41, 0)          // i32.const 0

	return code
}

// generateFdCloseHelper generates the close(fd) helper function bytecode
// Closes a file descriptor using WASI fd_close
// Params: fd (i32) - the file descriptor to close
// Returns: errno (0 on success)
func generateFdCloseHelper(fdCloseIdx int) []byte {
	var code []byte

	// fd_close(fd)
	code = append(code, OpLocalGet, 0)    // get fd param
	code = append(code, OpCall, byte(fdCloseIdx))

	// Return the errno
	return code
}

// generateReadHelper generates the read(fd, buf, len) helper function bytecode
// Reads from a file descriptor using WASI fd_read
// Params: fd (i32), buf (i32), len (i32)
// Returns: bytes read (i32), or -1 on error
// Uses memory at address 0 for iovec structure
func generateReadHelper(fdReadIdx int) []byte {
	var code []byte

	// Set up iovec at address 0:
	// iovec.buf = buf
	// iovec.buf_len = len
	code = append(code, 0x41, 0)           // i32.const 0 (iovec addr)
	code = append(code, OpLocalGet, 1)     // get buf param
	code = append(code, OpI32Store, 2, 0)  // store buf address

	code = append(code, 0x41, 4)           // i32.const 4 (iovec.buf_len offset)
	code = append(code, OpLocalGet, 2)     // get len param
	code = append(code, OpI32Store, 2, 0)  // store buf_len

	// fd_read(fd, 0, 1, 8)
	// fd=param0, iovs=0, iovs_len=1, nread=8
	code = append(code, OpLocalGet, 0)     // get fd param
	code = append(code, 0x41, 0)           // i32.const 0 (iovs)
	code = append(code, 0x41, 1)           // i32.const 1 (iovs_len)
	code = append(code, 0x41, 8)           // i32.const 8 (nread ptr)
	code = append(code, OpCall, byte(fdReadIdx))

	// Check errno (0 = success)
	code = append(code, 0x45)              // i32.eqz (test if errno == 0)
	code = append(code, OpIf, 0x7f)        // if errno == 0, return nread

	// Success: load nread from address 8
	code = append(code, 0x41, 8)           // i32.const 8
	code = append(code, OpI32Load, 2, 0)   // i32.load

	code = append(code, OpElse)            // else return -1
	code = append(code, 0x41, 0x7f)        // i32.const -1
	code = append(code, OpEnd)

	return code
}

// generateWriteHelper generates the write(fd, buf, len) helper function bytecode
// Writes to a file descriptor using WASI fd_write
// Params: fd (i32), buf (i32), len (i32)
// Returns: bytes written (i32), or -1 on error
// Uses memory at address 0 for iovec structure
func generateWriteHelper(fdWriteIdx int) []byte {
	var code []byte

	// Set up iovec at address 0:
	// iovec.buf = buf
	// iovec.buf_len = len
	code = append(code, 0x41, 0)           // i32.const 0 (iovec addr)
	code = append(code, OpLocalGet, 1)     // get buf param
	code = append(code, OpI32Store, 2, 0)  // store buf address

	code = append(code, 0x41, 4)           // i32.const 4 (iovec.buf_len offset)
	code = append(code, OpLocalGet, 2)     // get len param
	code = append(code, OpI32Store, 2, 0)  // store buf_len

	// fd_write(fd, 0, 1, 8)
	// fd=param0, iovs=0, iovs_len=1, nwritten=8
	code = append(code, OpLocalGet, 0)     // get fd param
	code = append(code, 0x41, 0)           // i32.const 0 (iovs)
	code = append(code, 0x41, 1)           // i32.const 1 (iovs_len)
	code = append(code, 0x41, 8)           // i32.const 8 (nwritten ptr)
	code = append(code, OpCall, byte(fdWriteIdx))

	// Check errno (0 = success)
	code = append(code, 0x45)              // i32.eqz (test if errno == 0)
	code = append(code, OpIf, 0x7f)        // if errno == 0, return nwritten

	// Success: load nwritten from address 8
	code = append(code, 0x41, 8)           // i32.const 8
	code = append(code, OpI32Load, 2, 0)   // i32.load

	code = append(code, OpElse)            // else return -1
	code = append(code, 0x41, 0x7f)        // i32.const -1
	code = append(code, OpEnd)

	return code
}

// generateSeekHelper generates the seek(fd, offset, whence, newoffset_ptr) helper function bytecode
// Seeks in a file descriptor using WASI fd_seek
// Params: fd (i32), offset (i32), whence (i32), newoffset_ptr (i32)
// Returns: errno (0 on success)
// Note: WASI fd_seek expects i64 offset but we only support i32
func generateSeekHelper(fdSeekIdx int) []byte {
	var code []byte

	// fd_seek(fd, offset, whence, newoffset_ptr)
	code = append(code, OpLocalGet, 0)     // get fd param
	code = append(code, OpLocalGet, 1)     // get offset param (i32)
	code = append(code, 0xac)              // i64.extend_i32_s (convert to i64)
	code = append(code, OpLocalGet, 2)     // get whence param
	code = append(code, OpLocalGet, 3)     // get newoffset_ptr param
	code = append(code, OpCall, byte(fdSeekIdx))

	// Return the errno
	return code
}

// generateSyncHelper generates the sync(fd) helper function bytecode
// Syncs a file descriptor using WASI fd_sync
// Params: fd (i32)
// Returns: errno (0 on success)
func generateSyncHelper(fdSyncIdx int) []byte {
	var code []byte

	// fd_sync(fd)
	code = append(code, OpLocalGet, 0)     // get fd param
	code = append(code, OpCall, byte(fdSyncIdx))

	// Return the errno
	return code
}

// generateOpenHelper generates the open(dirfd, path, path_len, oflags, rights_base, rights_inheriting, fdflags, fd_ptr) helper function bytecode
// Opens a file using WASI path_open
// Params: dirfd (i32), path (i32), path_len (i32), oflags (i32), rights_base (i32), rights_inheriting (i32), fdflags (i32), fd_ptr (i32)
// Returns: errno (0 on success), fd is written to fd_ptr
func generateOpenHelper(pathOpenIdx int) []byte {
	var code []byte

	// path_open(dirfd, dirflags=0, path, path_len, oflags, fs_rights_base, fs_rights_inheriting, fdflags, fd_ptr)
	// 8 params total in WASI
	code = append(code, OpLocalGet, 0)     // get dirfd param
	code = append(code, 0x41, 0)           // i32.const 0 (dirflags)
	code = append(code, OpLocalGet, 1)     // get path param
	code = append(code, OpLocalGet, 2)     // get path_len param
	code = append(code, OpLocalGet, 3)     // get oflags param
	code = append(code, OpLocalGet, 4)     // get rights_base param
	code = append(code, OpLocalGet, 5)     // get rights_inheriting param
	code = append(code, OpLocalGet, 6)     // get fdflags param
	code = append(code, OpLocalGet, 7)     // get fd_ptr param
	code = append(code, OpCall, byte(pathOpenIdx))

	// Return the errno
	return code
}

// generateMkdirHelper generates the mkdir(dirfd, path, path_len) helper function bytecode
// Creates a directory using WASI path_create_directory
// Params: dirfd (i32), path (i32), path_len (i32)
// Returns: errno (0 on success)
func generateMkdirHelper(pathCreateDirIdx int) []byte {
	var code []byte

	// path_create_directory(dirfd, path, path_len)
	code = append(code, OpLocalGet, 0)     // get dirfd param
	code = append(code, OpLocalGet, 1)     // get path param
	code = append(code, OpLocalGet, 2)     // get path_len param
	code = append(code, OpCall, byte(pathCreateDirIdx))

	// Return the errno
	return code
}

// generateRmdirHelper generates the rmdir(dirfd, path, path_len) helper function bytecode
// Removes a directory using WASI path_remove_directory
// Params: dirfd (i32), path (i32), path_len (i32)
// Returns: errno (0 on success)
func generateRmdirHelper(pathRemoveDirIdx int) []byte {
	var code []byte

	// path_remove_directory(dirfd, path, path_len)
	code = append(code, OpLocalGet, 0)     // get dirfd param
	code = append(code, OpLocalGet, 1)     // get path param
	code = append(code, OpLocalGet, 2)     // get path_len param
	code = append(code, OpCall, byte(pathRemoveDirIdx))

	// Return the errno
	return code
}

// generateUnlinkHelper generates the unlink(dirfd, path, path_len) helper function bytecode
// Removes a file using WASI path_unlink_file
// Params: dirfd (i32), path (i32), path_len (i32)
// Returns: errno (0 on success)
func generateUnlinkHelper(pathUnlinkIdx int) []byte {
	var code []byte

	// path_unlink_file(dirfd, path, path_len)
	code = append(code, OpLocalGet, 0)     // get dirfd param
	code = append(code, OpLocalGet, 1)     // get path param
	code = append(code, OpLocalGet, 2)     // get path_len param
	code = append(code, OpCall, byte(pathUnlinkIdx))

	// Return the errno
	return code
}

// generateRenameHelper generates the rename(old_dirfd, old_path, old_path_len, new_dirfd, new_path, new_path_len) helper function bytecode
// Renames a file using WASI path_rename
// Params: old_dirfd (i32), old_path (i32), old_path_len (i32), new_dirfd (i32), new_path (i32), new_path_len (i32)
// Returns: errno (0 on success)
func generateRenameHelper(pathRenameIdx int) []byte {
	var code []byte

	// path_rename(old_dirfd, old_path, old_path_len, new_dirfd, new_path, new_path_len)
	code = append(code, OpLocalGet, 0)     // get old_dirfd param
	code = append(code, OpLocalGet, 1)     // get old_path param
	code = append(code, OpLocalGet, 2)     // get old_path_len param
	code = append(code, OpLocalGet, 3)     // get new_dirfd param
	code = append(code, OpLocalGet, 4)     // get new_path param
	code = append(code, OpLocalGet, 5)     // get new_path_len param
	code = append(code, OpCall, byte(pathRenameIdx))

	// Return the errno
	return code
}

// generateStatHelper generates the stat(dirfd, flags, path, path_len, buf) helper function bytecode
// Gets file stats using WASI path_filestat_get
// Params: dirfd (i32), flags (i32), path (i32), path_len (i32), buf (i32)
// Returns: errno (0 on success)
func generateStatHelper(pathFilestatIdx int) []byte {
	var code []byte

	// path_filestat_get(dirfd, flags, path, path_len, buf)
	code = append(code, OpLocalGet, 0)     // get dirfd param
	code = append(code, OpLocalGet, 1)     // get flags param
	code = append(code, OpLocalGet, 2)     // get path param
	code = append(code, OpLocalGet, 3)     // get path_len param
	code = append(code, OpLocalGet, 4)     // get buf param
	code = append(code, OpCall, byte(pathFilestatIdx))

	// Return the errno
	return code
}

// generateGetArgsSizesHelper generates the get_args_sizes(argc_ptr, argv_buf_size_ptr) helper function bytecode
// Gets argument count and buffer size using WASI args_sizes_get
// Params: argc_ptr (i32), argv_buf_size_ptr (i32)
// Returns: errno (0 on success)
func generateGetArgsSizesHelper(argsSizesGetIdx int) []byte {
	var code []byte

	// args_sizes_get(argc_ptr, argv_buf_size_ptr)
	code = append(code, OpLocalGet, 0)     // get argc_ptr param
	code = append(code, OpLocalGet, 1)     // get argv_buf_size_ptr param
	code = append(code, OpCall, byte(argsSizesGetIdx))

	// Return the errno
	return code
}

// generateGetArgsHelper generates the get_args(argv_ptr, argv_buf_ptr) helper function bytecode
// Gets command-line arguments using WASI args_get
// Params: argv_ptr (i32), argv_buf_ptr (i32)
// Returns: errno (0 on success)
func generateGetArgsHelper(argsGetIdx int) []byte {
	var code []byte

	// args_get(argv_ptr, argv_buf_ptr)
	code = append(code, OpLocalGet, 0)     // get argv_ptr param
	code = append(code, OpLocalGet, 1)     // get argv_buf_ptr param
	code = append(code, OpCall, byte(argsGetIdx))

	// Return the errno
	return code
}

// generateGetEnvironSizesHelper generates the get_environ_sizes(environc_ptr, environ_buf_size_ptr) helper function bytecode
// Gets environment variable count and buffer size using WASI environ_sizes_get
// Params: environc_ptr (i32), environ_buf_size_ptr (i32)
// Returns: errno (0 on success)
func generateGetEnvironSizesHelper(environSizesGetIdx int) []byte {
	var code []byte

	// environ_sizes_get(environc_ptr, environ_buf_size_ptr)
	code = append(code, OpLocalGet, 0)     // get environc_ptr param
	code = append(code, OpLocalGet, 1)     // get environ_buf_size_ptr param
	code = append(code, OpCall, byte(environSizesGetIdx))

	// Return the errno
	return code
}

// generateGetEnvironHelper generates the get_environ(environ_ptr, environ_buf_ptr) helper function bytecode
// Gets environment variables using WASI environ_get
// Params: environ_ptr (i32), environ_buf_ptr (i32)
// Returns: errno (0 on success)
func generateGetEnvironHelper(environGetIdx int) []byte {
	var code []byte

	// environ_get(environ_ptr, environ_buf_ptr)
	code = append(code, OpLocalGet, 0)     // get environ_ptr param
	code = append(code, OpLocalGet, 1)     // get environ_buf_ptr param
	code = append(code, OpCall, byte(environGetIdx))

	// Return the errno
	return code
}

// generateArgcHelper generates the argc() helper function bytecode
// Returns the number of command-line arguments
// Uses memory at address 920-927 for the args_sizes_get output
func generateArgcHelper(argsSizesGetIdx int) []byte {
	var code []byte

	// args_sizes_get(920, 924) - store argc at 920, argv_buf_size at 924
	code = append(code, 0x41, 0x98, 0x07) // i32.const 920
	code = append(code, 0x41, 0x9c, 0x07) // i32.const 924
	code = append(code, OpCall, byte(argsSizesGetIdx))
	code = append(code, OpDrop) // discard errno

	// Load and return argc from 920
	code = append(code, 0x41, 0x98, 0x07) // i32.const 920
	code = append(code, OpI32Load, 0x02, 0x00) // i32.load align=4 offset=0

	return code
}

// generateEnvcHelper generates the envc() helper function bytecode
// Returns the number of environment variables
// Uses memory at address 928-935 for the environ_sizes_get output
func generateEnvcHelper(environSizesGetIdx int) []byte {
	var code []byte

	// environ_sizes_get(928, 932) - store environc at 928, environ_buf_size at 932
	code = append(code, 0x41, 0xa0, 0x07) // i32.const 928
	code = append(code, 0x41, 0xa4, 0x07) // i32.const 932
	code = append(code, OpCall, byte(environSizesGetIdx))
	code = append(code, OpDrop) // discard errno

	// Load and return environc from 928
	code = append(code, 0x41, 0xa0, 0x07) // i32.const 928
	code = append(code, OpI32Load, 0x02, 0x00) // i32.load align=4 offset=0

	return code
}

// generateStdinReadHelper generates the stdin_read(buf, len) helper function bytecode
// Reads from stdin (fd=0) into buffer
// Params: buf (i32), len (i32)
// Returns: bytes read (i32), or -1 on error
func generateStdinReadHelper(fdReadIdx int) []byte {
	var code []byte

	// Set up iovec at address 0: {buf, len}
	code = append(code, 0x41, 0x00)           // i32.const 0 (iovec address)
	code = append(code, OpLocalGet, 0)        // get buf param
	code = append(code, OpI32Store, 0x02, 0x00) // store buf at offset 0

	code = append(code, 0x41, 0x04)           // i32.const 4
	code = append(code, OpLocalGet, 1)        // get len param
	code = append(code, OpI32Store, 0x02, 0x00) // store len at offset 4

	// fd_read(0, 0, 1, 8) - fd=0 (stdin), iovec at 0, 1 iovec, result at 8
	code = append(code, 0x41, 0x00)           // i32.const 0 (stdin)
	code = append(code, 0x41, 0x00)           // i32.const 0 (iovec ptr)
	code = append(code, 0x41, 0x01)           // i32.const 1 (iovec count)
	code = append(code, 0x41, 0x08)           // i32.const 8 (result ptr)
	code = append(code, OpCall, byte(fdReadIdx))

	// If errno != 0, return -1
	code = append(code, OpIf, 0x7f)           // if (errno) : i32
	code = append(code, 0x41, 0x7f)           // i32.const -1
	code = append(code, OpElse)
	code = append(code, 0x41, 0x08)           // i32.const 8
	code = append(code, OpI32Load, 0x02, 0x00) // i32.load (bytes read)
	code = append(code, OpEnd)

	return code
}

// generateStdoutWriteHelper generates the stdout_write(buf, len) helper function bytecode
// Writes buffer to stdout (fd=1)
// Params: buf (i32), len (i32)
// Returns: bytes written (i32), or -1 on error
func generateStdoutWriteHelper(fdWriteIdx int) []byte {
	var code []byte

	// Set up iovec at address 0: {buf, len}
	code = append(code, 0x41, 0x00)           // i32.const 0 (iovec address)
	code = append(code, OpLocalGet, 0)        // get buf param
	code = append(code, OpI32Store, 0x02, 0x00) // store buf at offset 0

	code = append(code, 0x41, 0x04)           // i32.const 4
	code = append(code, OpLocalGet, 1)        // get len param
	code = append(code, OpI32Store, 0x02, 0x00) // store len at offset 4

	// fd_write(1, 0, 1, 8) - fd=1 (stdout), iovec at 0, 1 iovec, result at 8
	code = append(code, 0x41, 0x01)           // i32.const 1 (stdout)
	code = append(code, 0x41, 0x00)           // i32.const 0 (iovec ptr)
	code = append(code, 0x41, 0x01)           // i32.const 1 (iovec count)
	code = append(code, 0x41, 0x08)           // i32.const 8 (result ptr)
	code = append(code, OpCall, byte(fdWriteIdx))

	// If errno != 0, return -1
	code = append(code, OpIf, 0x7f)           // if (errno) : i32
	code = append(code, 0x41, 0x7f)           // i32.const -1
	code = append(code, OpElse)
	code = append(code, 0x41, 0x08)           // i32.const 8
	code = append(code, OpI32Load, 0x02, 0x00) // i32.load (bytes written)
	code = append(code, OpEnd)

	return code
}

// generateStderrWriteHelper generates the stderr_write(buf, len) helper function bytecode
// Writes buffer to stderr (fd=2)
// Params: buf (i32), len (i32)
// Returns: bytes written (i32), or -1 on error
func generateStderrWriteHelper(fdWriteIdx int) []byte {
	var code []byte

	// Set up iovec at address 0: {buf, len}
	code = append(code, 0x41, 0x00)           // i32.const 0 (iovec address)
	code = append(code, OpLocalGet, 0)        // get buf param
	code = append(code, OpI32Store, 0x02, 0x00) // store buf at offset 0

	code = append(code, 0x41, 0x04)           // i32.const 4
	code = append(code, OpLocalGet, 1)        // get len param
	code = append(code, OpI32Store, 0x02, 0x00) // store len at offset 4

	// fd_write(2, 0, 1, 8) - fd=2 (stderr), iovec at 0, 1 iovec, result at 8
	code = append(code, 0x41, 0x02)           // i32.const 2 (stderr)
	code = append(code, 0x41, 0x00)           // i32.const 0 (iovec ptr)
	code = append(code, 0x41, 0x01)           // i32.const 1 (iovec count)
	code = append(code, 0x41, 0x08)           // i32.const 8 (result ptr)
	code = append(code, OpCall, byte(fdWriteIdx))

	// If errno != 0, return -1
	code = append(code, OpIf, 0x7f)           // if (errno) : i32
	code = append(code, 0x41, 0x7f)           // i32.const -1
	code = append(code, OpElse)
	code = append(code, 0x41, 0x08)           // i32.const 8
	code = append(code, OpI32Load, 0x02, 0x00) // i32.load (bytes written)
	code = append(code, OpEnd)

	return code
}

// generateFdstatHelper generates the fdstat(fd, buf) helper function bytecode
// Gets file descriptor stats using WASI fd_fdstat_get
// Params: fd (i32), buf (i32)
// Returns: errno (0 on success)
func generateFdstatHelper(fdFdstatGetIdx int) []byte {
	var code []byte

	// fd_fdstat_get(fd, buf)
	code = append(code, OpLocalGet, 0)     // get fd param
	code = append(code, OpLocalGet, 1)     // get buf param
	code = append(code, OpCall, byte(fdFdstatGetIdx))

	// Return the errno
	return code
}

// generateFstatHelper generates the fstat(fd, buf) helper function bytecode
// Gets file stats by file descriptor using WASI fd_filestat_get
// Params: fd (i32), buf (i32)
// Returns: errno (0 on success)
func generateFstatHelper(fdFilestatGetIdx int) []byte {
	var code []byte

	// fd_filestat_get(fd, buf)
	code = append(code, OpLocalGet, 0)     // get fd param
	code = append(code, OpLocalGet, 1)     // get buf param
	code = append(code, OpCall, byte(fdFilestatGetIdx))

	// Return the errno
	return code
}

// generateReaddirHelper generates the readdir(fd, buf, buf_len, cookie) helper function bytecode
// Reads directory entries using WASI fd_readdir
// Params: fd (i32), buf (i32), buf_len (i32), cookie (i32)
// Returns: errno (0 on success)
// Note: WASI fd_readdir expects i64 cookie but we only support i32
func generateReaddirHelper(fdReaddirIdx int) []byte {
	var code []byte

	// fd_readdir(fd, buf, buf_len, cookie)
	code = append(code, OpLocalGet, 0)     // get fd param
	code = append(code, OpLocalGet, 1)     // get buf param
	code = append(code, OpLocalGet, 2)     // get buf_len param
	code = append(code, OpLocalGet, 3)     // get cookie param (i32)
	code = append(code, 0xac)              // i64.extend_i32_s (convert to i64)
	code = append(code, OpCall, byte(fdReaddirIdx))

	// Return the errno
	return code
}

// generateYieldHelper generates the yield() helper function bytecode
// Yields to the scheduler using WASI sched_yield
// Returns: errno (0 on success)
func generateYieldHelper(schedYieldIdx int) []byte {
	var code []byte

	// sched_yield()
	code = append(code, OpCall, byte(schedYieldIdx))

	// Return the errno
	return code
}

// generateRaiseHelper generates the raise(sig) helper function bytecode
// Raises a signal using WASI proc_raise
// Params: sig (i32)
// Returns: errno (0 on success)
func generateRaiseHelper(procRaiseIdx int) []byte {
	var code []byte

	// proc_raise(sig)
	code = append(code, OpLocalGet, 0)     // get sig param
	code = append(code, OpCall, byte(procRaiseIdx))

	// Return the errno
	return code
}

// generateSetFdFlagsHelper generates the set_fd_flags(fd, flags) helper function bytecode
// Sets file descriptor flags using WASI fd_fdstat_set_flags
// Params: fd (i32), flags (i32)
// Returns: errno (0 on success)
func generateSetFdFlagsHelper(fdFdstatSetFlagsIdx int) []byte {
	var code []byte

	// fd_fdstat_set_flags(fd, flags)
	code = append(code, OpLocalGet, 0)     // get fd param
	code = append(code, OpLocalGet, 1)     // get flags param
	code = append(code, OpCall, byte(fdFdstatSetFlagsIdx))

	// Return the errno
	return code
}

// generateDatasyncHelper generates the datasync(fd) helper function bytecode
// Syncs data to disk using WASI fd_datasync
// Params: fd (i32)
// Returns: errno (0 on success)
func generateDatasyncHelper(fdDatasyncIdx int) []byte {
	var code []byte

	// fd_datasync(fd)
	code = append(code, OpLocalGet, 0)     // get fd param
	code = append(code, OpCall, byte(fdDatasyncIdx))

	// Return the errno
	return code
}

// generateAllocateHelper generates the allocate(fd, offset, len) helper function bytecode
// Allocates space for a file using WASI fd_allocate
// Params: fd (i32), offset (i32), len (i32)
// Returns: errno (0 on success)
// Note: WASI fd_allocate expects i64 offset/len but we only support i32
func generateAllocateHelper(fdAllocateIdx int) []byte {
	var code []byte

	// fd_allocate(fd, offset, len)
	code = append(code, OpLocalGet, 0)     // get fd param
	code = append(code, OpLocalGet, 1)     // get offset param (i32)
	code = append(code, 0xac)              // i64.extend_i32_s (convert to i64)
	code = append(code, OpLocalGet, 2)     // get len param (i32)
	code = append(code, 0xac)              // i64.extend_i32_s (convert to i64)
	code = append(code, OpCall, byte(fdAllocateIdx))

	// Return the errno
	return code
}

// generateAdviseHelper generates the advise(fd, offset, len, advice) helper function bytecode
// Provides file advice using WASI fd_advise
// Params: fd (i32), offset (i32), len (i32), advice (i32)
// Returns: errno (0 on success)
// Note: WASI fd_advise expects i64 offset/len but we only support i32
func generateAdviseHelper(fdAdviseIdx int) []byte {
	var code []byte

	// fd_advise(fd, offset, len, advice)
	code = append(code, OpLocalGet, 0)     // get fd param
	code = append(code, OpLocalGet, 1)     // get offset param (i32)
	code = append(code, 0xac)              // i64.extend_i32_s (convert to i64)
	code = append(code, OpLocalGet, 2)     // get len param (i32)
	code = append(code, 0xac)              // i64.extend_i32_s (convert to i64)
	code = append(code, OpLocalGet, 3)     // get advice param
	code = append(code, OpCall, byte(fdAdviseIdx))

	// Return the errno
	return code
}

// generateLinkHelper generates the link(old_fd, old_flags, old_path, old_path_len, new_fd, new_path, new_path_len) helper function bytecode
// Creates a hard link using WASI path_link
// Params: old_fd (i32), old_flags (i32), old_path (i32), old_path_len (i32), new_fd (i32), new_path (i32), new_path_len (i32)
// Returns: errno (0 on success)
func generateLinkHelper(pathLinkIdx int) []byte {
	var code []byte

	// path_link(old_fd, old_flags, old_path, old_path_len, new_fd, new_path, new_path_len)
	code = append(code, OpLocalGet, 0)     // get old_fd param
	code = append(code, OpLocalGet, 1)     // get old_flags param
	code = append(code, OpLocalGet, 2)     // get old_path param
	code = append(code, OpLocalGet, 3)     // get old_path_len param
	code = append(code, OpLocalGet, 4)     // get new_fd param
	code = append(code, OpLocalGet, 5)     // get new_path param
	code = append(code, OpLocalGet, 6)     // get new_path_len param
	code = append(code, OpCall, byte(pathLinkIdx))

	// Return the errno
	return code
}

// generateTellHelper generates the tell() helper function bytecode
// Params: fd (i32), offset_ptr (i32)
// Returns: errno (0 on success)
func generateTellHelper(fdTellIdx int) []byte {
	var code []byte

	// fd_tell(fd, offset_ptr)
	code = append(code, OpLocalGet, 0) // get fd param
	code = append(code, OpLocalGet, 1) // get offset_ptr param
	code = append(code, OpCall, byte(fdTellIdx))

	// Return the errno
	return code
}

// generateSymlinkHelper generates the symlink() helper function bytecode
// Params: old_path (i32), old_path_len (i32), fd (i32), new_path (i32), new_path_len (i32)
// Returns: errno (0 on success)
func generateSymlinkHelper(pathSymlinkIdx int) []byte {
	var code []byte

	// path_symlink(old_path, old_path_len, fd, new_path, new_path_len)
	code = append(code, OpLocalGet, 0) // get old_path param
	code = append(code, OpLocalGet, 1) // get old_path_len param
	code = append(code, OpLocalGet, 2) // get fd param
	code = append(code, OpLocalGet, 3) // get new_path param
	code = append(code, OpLocalGet, 4) // get new_path_len param
	code = append(code, OpCall, byte(pathSymlinkIdx))

	// Return the errno
	return code
}

// generateReadlinkHelper generates the readlink() helper function bytecode
// Params: fd (i32), path (i32), path_len (i32), buf (i32), buf_len (i32), bufused_ptr (i32)
// Returns: errno (0 on success)
func generateReadlinkHelper(pathReadlinkIdx int) []byte {
	var code []byte

	// path_readlink(fd, path, path_len, buf, buf_len, bufused_ptr)
	code = append(code, OpLocalGet, 0) // get fd param
	code = append(code, OpLocalGet, 1) // get path param
	code = append(code, OpLocalGet, 2) // get path_len param
	code = append(code, OpLocalGet, 3) // get buf param
	code = append(code, OpLocalGet, 4) // get buf_len param
	code = append(code, OpLocalGet, 5) // get bufused_ptr param
	code = append(code, OpCall, byte(pathReadlinkIdx))

	// Return the errno
	return code
}

// generatePrestatGetHelper generates the prestat_get() helper function bytecode
// Params: fd (i32), buf (i32)
// Returns: errno (0 on success)
func generatePrestatGetHelper(fdPrestatGetIdx int) []byte {
	var code []byte

	// fd_prestat_get(fd, buf)
	code = append(code, OpLocalGet, 0) // get fd param
	code = append(code, OpLocalGet, 1) // get buf param
	code = append(code, OpCall, byte(fdPrestatGetIdx))

	// Return the errno
	return code
}

// generatePrestatDirNameHelper generates the prestat_dir_name() helper function bytecode
// Params: fd (i32), path (i32), path_len (i32)
// Returns: errno (0 on success)
func generatePrestatDirNameHelper(fdPrestatDirNameIdx int) []byte {
	var code []byte

	// fd_prestat_dir_name(fd, path, path_len)
	code = append(code, OpLocalGet, 0) // get fd param
	code = append(code, OpLocalGet, 1) // get path param
	code = append(code, OpLocalGet, 2) // get path_len param
	code = append(code, OpCall, byte(fdPrestatDirNameIdx))

	// Return the errno
	return code
}

// generateSockRecvHelper generates the sock_recv() helper function bytecode
// Params: fd (i32), ri_data (i32), ri_data_len (i32), ri_flags (i32), ro_datalen (i32), ro_flags (i32)
// Returns: errno (0 on success)
func generateSockRecvHelper(sockRecvIdx int) []byte {
	var code []byte

	// sock_recv(fd, ri_data, ri_data_len, ri_flags, ro_datalen, ro_flags)
	code = append(code, OpLocalGet, 0) // get fd param
	code = append(code, OpLocalGet, 1) // get ri_data param
	code = append(code, OpLocalGet, 2) // get ri_data_len param
	code = append(code, OpLocalGet, 3) // get ri_flags param
	code = append(code, OpLocalGet, 4) // get ro_datalen param
	code = append(code, OpLocalGet, 5) // get ro_flags param
	code = append(code, OpCall, byte(sockRecvIdx))

	// Return the errno
	return code
}

// generateSockSendHelper generates the sock_send() helper function bytecode
// Params: fd (i32), si_data (i32), si_data_len (i32), si_flags (i32), so_datalen (i32)
// Returns: errno (0 on success)
func generateSockSendHelper(sockSendIdx int) []byte {
	var code []byte

	// sock_send(fd, si_data, si_data_len, si_flags, so_datalen)
	code = append(code, OpLocalGet, 0) // get fd param
	code = append(code, OpLocalGet, 1) // get si_data param
	code = append(code, OpLocalGet, 2) // get si_data_len param
	code = append(code, OpLocalGet, 3) // get si_flags param
	code = append(code, OpLocalGet, 4) // get so_datalen param
	code = append(code, OpCall, byte(sockSendIdx))

	// Return the errno
	return code
}

// generateSockShutdownHelper generates the sock_shutdown() helper function bytecode
// Params: fd (i32), how (i32)
// Returns: errno (0 on success)
func generateSockShutdownHelper(sockShutdownIdx int) []byte {
	var code []byte

	// sock_shutdown(fd, how)
	code = append(code, OpLocalGet, 0) // get fd param
	code = append(code, OpLocalGet, 1) // get how param
	code = append(code, OpCall, byte(sockShutdownIdx))

	// Return the errno
	return code
}

// HTTP helpers for Component Model (wasi:http)
// These work with wasi:http/outgoing-handler when running in a component-aware runtime

// generateHTTPGetHelper generates the http_get() helper function bytecode
// Params: url_ptr (i32), url_len (i32), response_ptr (i32)
// Returns: status code (i32), or negative error code
// Uses memory at response_ptr for response body (first 4 bytes = length, then data)
func generateHTTPGetHelper() []byte {
	var code []byte

	// For now, this is a stub that returns -1 (not supported in preview1)
	// When compiled as a component, the runtime provides the implementation
	code = append(code, 0x41) // i32.const
	code = append(code, 0x7f) // -1 (signed LEB128)

	return code
}

// generateHTTPPostHelper generates the http_post() helper function bytecode
// Params: url_ptr (i32), url_len (i32), body_ptr (i32), body_len (i32), response_ptr (i32)
// Returns: status code (i32), or negative error code
func generateHTTPPostHelper() []byte {
	var code []byte

	// Stub - returns -1 (not supported in preview1)
	code = append(code, 0x41) // i32.const
	code = append(code, 0x7f) // -1

	return code
}

// generateHTTPRequestHelper generates the http_request() helper function bytecode
// Params: method (i32), url_ptr (i32), url_len (i32), headers_ptr (i32), headers_len (i32),
//         body_ptr (i32), body_len (i32), response_ptr (i32)
// Returns: status code (i32), or negative error code
func generateHTTPRequestHelper() []byte {
	var code []byte

	// Stub - returns -1 (not supported in preview1)
	code = append(code, 0x41) // i32.const
	code = append(code, 0x7f) // -1

	return code
}

// generateTimeHelper generates the time() helper function bytecode
// Returns the current wall clock time in seconds as i32
// Uses memory at address 904 for the 8-byte timestamp buffer
func generateTimeHelper(clockTimeGetIdx int) []byte {
	var code []byte

	// clock_time_get(0, 0, 904) - get realtime clock with best precision, store at 904
	code = append(code, 0x41, 0)           // i32.const 0 (CLOCK_REALTIME)
	code = append(code, 0x42, 0)           // i64.const 0 (precision - best available)
	code = append(code, 0x41, 0x88, 0x07)  // i32.const 904 (LEB128)
	code = append(code, OpCall, byte(clockTimeGetIdx))
	code = append(code, 0x1a)              // drop result (errno)

	// Load the timestamp as i64
	code = append(code, 0x41, 0x88, 0x07)  // i32.const 904
	code = append(code, OpI64Load, 3, 0)   // i64.load (align=3 for 8 bytes)

	// Divide by 1,000,000,000 to convert nanoseconds to seconds
	code = append(code, 0x42, 0x80, 0x94, 0xeb, 0xdc, 0x03) // i64.const 1000000000 (LEB128)
	code = append(code, 0x7f)                               // i64.div_s

	// Wrap to i32
	code = append(code, 0xa7) // i32.wrap_i64

	return code
}

// generateMillisHelper generates the millis() helper function bytecode
// Returns the current wall clock time in milliseconds as i32
// Uses memory at address 904 for the 8-byte timestamp buffer
func generateMillisHelper(clockTimeGetIdx int) []byte {
	var code []byte

	// clock_time_get(0, 0, 904) - get realtime clock with best precision, store at 904
	code = append(code, 0x41, 0)           // i32.const 0 (CLOCK_REALTIME)
	code = append(code, 0x42, 0)           // i64.const 0 (precision - best available)
	code = append(code, 0x41, 0x88, 0x07)  // i32.const 904 (LEB128)
	code = append(code, OpCall, byte(clockTimeGetIdx))
	code = append(code, 0x1a)              // drop result (errno)

	// Load the timestamp as i64
	code = append(code, 0x41, 0x88, 0x07)  // i32.const 904
	code = append(code, OpI64Load, 3, 0)   // i64.load (align=3 for 8 bytes)

	// Divide by 1,000,000 to convert nanoseconds to milliseconds
	code = append(code, 0x42, 0xc0, 0x84, 0x3d) // i64.const 1000000 (LEB128)
	code = append(code, 0x7f)                   // i64.div_s

	// Wrap to i32
	code = append(code, 0xa7) // i32.wrap_i64

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

// generateAsyncSleepHelper generates the async_sleep(ms) helper function bytecode
// Simulates async sleep using poll_oneoff
// For now, this is just a stub that returns immediately
// In a real implementation, this would use WASI async I/O
func generateAsyncSleepHelper() []byte {
	var code []byte
	// For now, just return 0 (success)
	code = append(code, 0x41, 0) // i32.const 0
	return code
}

// generateAsyncReadHelper generates the async_read(fd, buf, len) helper function bytecode
// Simulates async file read using poll_oneoff
// For now, this is just a stub that calls fd_read synchronously
func generateAsyncReadHelper(fdReadIdx int) []byte {
	var code []byte
	// For now, just call fd_read synchronously
	// Params: fd, iovs, iovs_len, nread
	code = append(code, OpLocalGet, 0) // fd
	code = append(code, OpLocalGet, 1) // buf (iovs.buf)
	code = append(code, 0x41, 0)      // iovs address
	code = append(code, OpI32Store, 2, 0) // store buf at iovs.buf
	code = append(code, OpLocalGet, 2) // len
	code = append(code, 0x41, 4)      // iovs.len address
	code = append(code, OpI32Store, 2, 0) // store len at iovs.len
	code = append(code, OpLocalGet, 0) // fd
	code = append(code, 0x41, 0)      // iovs address
	code = append(code, 0x41, 1)      // iovs_len = 1
	code = append(code, 0x41, 8)      // nread address
	code = append(code, OpCall, byte(fdReadIdx))
	// Return nread
	code = append(code, 0x41, 8)      // nread address
	code = append(code, OpI32Load, 2, 0)
	return code
}

// generateAsyncRuntimeInitHelper generates the async runtime initialization code
// Sets up memory for async state and continuations
func generateAsyncRuntimeInitHelper() []byte {
	var code []byte
	// For now, just return 0 (success)
	// In a real implementation, this would initialize async runtime state
	code = append(code, 0x41, 0) // i32.const 0
	return code
}

// generateAsyncYieldHelper generates code to yield execution and suspend
// This is the core of async/await - saves state and returns to caller
func generateAsyncYieldHelper() []byte {
	var code []byte
	// For now, just return 0 (ready)
	// In a real implementation, this would:
	// 1. Save current execution state
	// 2. Set up continuation
	// 3. Return "not ready" status
	code = append(code, 0x41, 0) // i32.const 0 (ready)
	return code
}

// generateAsyncResumeHelper generates code to resume a suspended async function
// Restores state and continues execution
func generateAsyncResumeHelper() []byte {
	var code []byte
	// For now, just return 0 (success)
	// In a real implementation, this would:
	// 1. Restore saved execution state
	// 2. Jump to continuation point
	// 3. Return result
	code = append(code, 0x41, 0) // i32.const 0 (success)
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
	// Collect struct definitions
	structs := make(map[string]*StructInfo)
	for _, s := range file.Structs {
		info := &StructInfo{
			Name:    s.Name,
			Offsets: make(map[string]int),
		}
		offset := 0
		for _, f := range s.Fields {
			size := 4 // default to i32 size
			switch f.Type {
			case "i64", "f64":
				size = 8
			}
			info.Fields = append(info.Fields, StructFieldInfo{
				Name:   f.Name,
				Type:   f.Type,
				Size:   size,
				Offset: offset,
			})
			info.Offsets[f.Name] = offset
			offset += size
		}
		info.Size = offset
		structs[s.Name] = info
	}

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
	// wasiImportInfo contains numParams and whether the function has a return value
	type wasiImportInfo struct {
		numParams int
		hasResult bool
	}
	wasiImports := map[string]wasiImportInfo{
		"fd_write":              {4, true},
		"fd_read":               {4, true},
		"args_get":              {2, true},
		"args_sizes_get":        {2, true},
		"environ_get":           {2, true},
		"environ_sizes_get":     {2, true},
		"proc_exit":             {1, false}, // void - terminates process
		"random_get":            {2, true},
		"path_open":             {8, true},
		"fd_close":              {1, true},
		"fd_prestat_get":        {2, true},
		"fd_prestat_dir_name":   {3, true},
		"fd_seek":               {4, true},
		"fd_tell":               {2, true},
		"fd_fdstat_get":         {2, true},
		"fd_fdstat_set_flags":   {2, true},
		"fd_filestat_get":       {2, true},
		"path_create_directory": {3, true},
		"path_remove_directory": {3, true},
		"path_unlink_file":      {3, true},
		"path_rename":           {5, true},
		"path_filestat_get":     {4, true},
		"fd_readdir":            {4, true},
		"fd_sync":               {1, true},
		"fd_datasync":           {1, true},
		"fd_allocate":           {3, true},
		"fd_advise":             {4, true},
		"path_link":             {6, true},
		"path_symlink":          {4, true},
		"path_readlink":         {5, true},
		"sched_yield":           {0, true},
		"clock_res_get":         {2, true},
		"clock_time_get":        {3, true},
		"poll_oneoff":           {4, true},
		"proc_raise":            {1, true},
		"sock_recv":             {6, true},
		"sock_send":             {5, true},
		"sock_shutdown":         {2, true},
		// WASI async I/O functions (for future use)
		"fd_pread":              {5, true},  // Async pread
		"fd_pwrite":             {5, true},  // Async pwrite
	}

	usedImports := make(map[string]bool)
	for _, fn := range file.Fns {
		collectCalls(fn.Body, usedImports)
	}

	// Check if any functions are async
	hasAsyncFunctions := false
	for _, fn := range file.Fns {
		if fn.Async {
			hasAsyncFunctions = true
			break
		}
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

	// Ensure random_get is imported if random is used
	needsRandomEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "random") {
			needsRandomEarly = true
			break
		}
	}
	if needsRandomEarly {
		usedImports["random_get"] = true
	}

	// Ensure proc_exit is imported if exit is used
	needsExitEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "exit") {
			needsExitEarly = true
			break
		}
	}
	if needsExitEarly {
		usedImports["proc_exit"] = true
	}

	// Ensure fd_close is imported if close is used
	needsFdCloseEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "close") {
			needsFdCloseEarly = true
			break
		}
	}
	if needsFdCloseEarly {
		usedImports["fd_close"] = true
	}

	// Ensure fd_read is imported if read is used
	needsReadEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "read") {
			needsReadEarly = true
			break
		}
	}
	if needsReadEarly {
		usedImports["fd_read"] = true
	}

	// Ensure fd_write is imported if write is used
	needsWriteEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "write") {
			needsWriteEarly = true
			break
		}
	}
	if needsWriteEarly {
		usedImports["fd_write"] = true
	}

	// Ensure fd_seek is imported if seek is used
	needsSeekEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "seek") {
			needsSeekEarly = true
			break
		}
	}
	if needsSeekEarly {
		usedImports["fd_seek"] = true
	}

	// Ensure fd_sync is imported if sync is used
	needsSyncEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "sync") {
			needsSyncEarly = true
			break
		}
	}
	if needsSyncEarly {
		usedImports["fd_sync"] = true
	}

	// Ensure path_open is imported if open is used
	needsOpenEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "open") {
			needsOpenEarly = true
			break
		}
	}
	if needsOpenEarly {
		usedImports["path_open"] = true
	}

	// Ensure path_create_directory is imported if mkdir is used
	needsMkdirEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "mkdir") {
			needsMkdirEarly = true
			break
		}
	}
	if needsMkdirEarly {
		usedImports["path_create_directory"] = true
	}

	// Ensure path_remove_directory is imported if rmdir is used
	needsRmdirEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "rmdir") {
			needsRmdirEarly = true
			break
		}
	}
	if needsRmdirEarly {
		usedImports["path_remove_directory"] = true
	}

	// Ensure path_unlink_file is imported if unlink is used
	needsUnlinkEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "unlink") {
			needsUnlinkEarly = true
			break
		}
	}
	if needsUnlinkEarly {
		usedImports["path_unlink_file"] = true
	}

	// Ensure path_rename is imported if rename is used
	needsRenameEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "rename") {
			needsRenameEarly = true
			break
		}
	}
	if needsRenameEarly {
		usedImports["path_rename"] = true
	}

	// Ensure path_filestat_get is imported if stat is used
	needsStatEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "stat") {
			needsStatEarly = true
			break
		}
	}
	if needsStatEarly {
		usedImports["path_filestat_get"] = true
	}

	// Ensure args_sizes_get is imported if get_args_sizes is used
	needsGetArgsSizesEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "get_args_sizes") {
			needsGetArgsSizesEarly = true
			break
		}
	}
	if needsGetArgsSizesEarly {
		usedImports["args_sizes_get"] = true
	}

	// Ensure args_get is imported if get_args is used
	needsGetArgsEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "get_args") {
			needsGetArgsEarly = true
			break
		}
	}
	if needsGetArgsEarly {
		usedImports["args_get"] = true
	}

	// Ensure environ_sizes_get is imported if get_environ_sizes is used
	needsGetEnvironSizesEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "get_environ_sizes") {
			needsGetEnvironSizesEarly = true
			break
		}
	}
	if needsGetEnvironSizesEarly {
		usedImports["environ_sizes_get"] = true
	}

	// Ensure environ_get is imported if get_environ is used
	needsGetEnvironEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "get_environ") {
			needsGetEnvironEarly = true
			break
		}
	}
	if needsGetEnvironEarly {
		usedImports["environ_get"] = true
	}

	// Check if argc is used
	needsArgcEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "argc") {
			needsArgcEarly = true
			break
		}
	}
	if needsArgcEarly {
		usedImports["args_sizes_get"] = true
	}

	// Check if envc is used
	needsEnvcEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "envc") {
			needsEnvcEarly = true
			break
		}
	}
	if needsEnvcEarly {
		usedImports["environ_sizes_get"] = true
	}

	// Check if stdin_read is used
	needsStdinReadEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "stdin_read") {
			needsStdinReadEarly = true
			break
		}
	}
	if needsStdinReadEarly {
		usedImports["fd_read"] = true
	}

	// Check if stdout_write is used
	needsStdoutWriteEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "stdout_write") {
			needsStdoutWriteEarly = true
			break
		}
	}
	if needsStdoutWriteEarly {
		usedImports["fd_write"] = true
	}

	// Check if stderr_write is used
	needsStderrWriteEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "stderr_write") {
			needsStderrWriteEarly = true
			break
		}
	}
	if needsStderrWriteEarly {
		usedImports["fd_write"] = true
	}

	// Ensure fd_fdstat_get is imported if fdstat is used
	needsFdstatEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "fdstat") {
			needsFdstatEarly = true
			break
		}
	}
	if needsFdstatEarly {
		usedImports["fd_fdstat_get"] = true
	}

	// Ensure fd_filestat_get is imported if fstat is used
	needsFstatEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "fstat") {
			needsFstatEarly = true
			break
		}
	}
	if needsFstatEarly {
		usedImports["fd_filestat_get"] = true
	}

	// Ensure fd_readdir is imported if readdir is used
	needsReaddirEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "readdir") {
			needsReaddirEarly = true
			break
		}
	}
	if needsReaddirEarly {
		usedImports["fd_readdir"] = true
	}

	// Ensure sched_yield is imported if yield is used
	needsYieldEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "yield") {
			needsYieldEarly = true
			break
		}
	}
	if needsYieldEarly {
		usedImports["sched_yield"] = true
	}

	// Ensure proc_raise is imported if raise is used
	needsRaiseEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "raise") {
			needsRaiseEarly = true
			break
		}
	}
	if needsRaiseEarly {
		usedImports["proc_raise"] = true
	}

	// Ensure fd_fdstat_set_flags is imported if set_fd_flags is used
	needsSetFdFlagsEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "set_fd_flags") {
			needsSetFdFlagsEarly = true
			break
		}
	}
	if needsSetFdFlagsEarly {
		usedImports["fd_fdstat_set_flags"] = true
	}

	// Ensure fd_datasync is imported if datasync is used
	needsDatasyncEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "datasync") {
			needsDatasyncEarly = true
			break
		}
	}
	if needsDatasyncEarly {
		usedImports["fd_datasync"] = true
	}

	// Ensure fd_allocate is imported if allocate is used
	needsAllocateEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "allocate") {
			needsAllocateEarly = true
			break
		}
	}
	if needsAllocateEarly {
		usedImports["fd_allocate"] = true
	}

	// Ensure fd_advise is imported if advise is used
	needsAdviseEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "advise") {
			needsAdviseEarly = true
			break
		}
	}
	if needsAdviseEarly {
		usedImports["fd_advise"] = true
	}

	// Ensure path_link is imported if link is used
	needsLinkEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "link") {
			needsLinkEarly = true
			break
		}
	}
	if needsLinkEarly {
		usedImports["path_link"] = true
	}

	// Ensure fd_tell is imported if tell is used
	needsTellEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "tell") {
			needsTellEarly = true
			break
		}
	}
	if needsTellEarly {
		usedImports["fd_tell"] = true
	}

	// Ensure path_symlink is imported if symlink is used
	needsSymlinkEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "symlink") {
			needsSymlinkEarly = true
			break
		}
	}
	if needsSymlinkEarly {
		usedImports["path_symlink"] = true
	}

	// Ensure path_readlink is imported if readlink is used
	needsReadlinkEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "readlink") {
			needsReadlinkEarly = true
			break
		}
	}
	if needsReadlinkEarly {
		usedImports["path_readlink"] = true
	}

	// Ensure fd_prestat_get is imported if prestat_get is used
	needsPrestatGetEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "prestat_get") {
			needsPrestatGetEarly = true
			break
		}
	}
	if needsPrestatGetEarly {
		usedImports["fd_prestat_get"] = true
	}

	// Ensure fd_prestat_dir_name is imported if prestat_dir_name is used
	needsPrestatDirNameEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "prestat_dir_name") {
			needsPrestatDirNameEarly = true
			break
		}
	}
	if needsPrestatDirNameEarly {
		usedImports["fd_prestat_dir_name"] = true
	}

	// Ensure sock_recv is imported if sock_recv is used
	needsSockRecvEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "sock_recv") {
			needsSockRecvEarly = true
			break
		}
	}
	if needsSockRecvEarly {
		usedImports["sock_recv"] = true
	}

	// Ensure sock_send is imported if sock_send is used
	needsSockSendEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "sock_send") {
			needsSockSendEarly = true
			break
		}
	}
	if needsSockSendEarly {
		usedImports["sock_send"] = true
	}

	// Ensure sock_shutdown is imported if sock_shutdown is used
	needsSockShutdownEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "sock_shutdown") {
			needsSockShutdownEarly = true
			break
		}
	}
	if needsSockShutdownEarly {
		usedImports["sock_shutdown"] = true
	}

	// Check if HTTP builtins are used (Component Model - no WASI import needed)
	needsHTTPGetEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "http_get") {
			needsHTTPGetEarly = true
			break
		}
	}

	needsHTTPPostEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "http_post") {
			needsHTTPPostEarly = true
			break
		}
	}

	needsHTTPRequestEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "http_request") {
			needsHTTPRequestEarly = true
			break
		}
	}

	// Ensure clock_time_get is imported if time is used
	needsTimeEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "time") {
			needsTimeEarly = true
			break
		}
	}
	if needsTimeEarly {
		usedImports["clock_time_get"] = true
	}

	// Ensure clock_time_get is imported if millis is used
	needsMillisEarly := false
	for _, fn := range file.Fns {
		if usesBuiltin(fn.Body, "millis") {
			needsMillisEarly = true
			break
		}
	}
	if needsMillisEarly {
		usedImports["clock_time_get"] = true
	}

	// Always include proc_exit for WASI runtime compatibility (e.g., Bun, wasmtime)
	// WASI runtimes require at least one import from wasi_snapshot_preview1
	usedImports["proc_exit"] = true

	// Add WASI imports first
	for name := range usedImports {
		if info, ok := wasiImports[name]; ok {
			// clock_time_get has special param types: (i32, i64, i32)
			if name == "clock_time_get" {
				m.AddImportWithTypes("wasi_snapshot_preview1", name, info.numParams, []byte{I32, I64, I32}, info.hasResult)
			} else {
				m.AddImport("wasi_snapshot_preview1", name, info.numParams, info.hasResult)
			}
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
	needsMod := false
	needsPow := false
	needsSqrt := false
	needsAnd := false
	needsOr := false
	needsXor := false
	needsShl := false
	needsShr := false
	needsClz := false
	needsCtz := false
	needsPopcnt := false
	needsRotl := false
	needsRotr := false
	needsStrEq := false
	needsStrCopy := false
	needsReadChar := false
	needsWriteChar := false
	needsRandom := false
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
		if usesBuiltin(fn.Body, "mod") {
			needsMod = true
		}
		if usesBuiltin(fn.Body, "pow") {
			needsPow = true
		}
		if usesBuiltin(fn.Body, "sqrt") {
			needsSqrt = true
		}
		if usesBuiltin(fn.Body, "and") {
			needsAnd = true
		}
		if usesBuiltin(fn.Body, "or") {
			needsOr = true
		}
		if usesBuiltin(fn.Body, "xor") {
			needsXor = true
		}
		if usesBuiltin(fn.Body, "shl") {
			needsShl = true
		}
		if usesBuiltin(fn.Body, "shr") {
			needsShr = true
		}
		if usesBuiltin(fn.Body, "clz") {
			needsClz = true
		}
		if usesBuiltin(fn.Body, "ctz") {
			needsCtz = true
		}
		if usesBuiltin(fn.Body, "popcnt") {
			needsPopcnt = true
		}
		if usesBuiltin(fn.Body, "rotl") {
			needsRotl = true
		}
		if usesBuiltin(fn.Body, "rotr") {
			needsRotr = true
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
		if usesBuiltin(fn.Body, "random") {
			needsRandom = true
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
	if needsMod {
		helperCount++
	}
	if needsPow {
		helperCount++
	}
	if needsSqrt {
		helperCount++
	}
	if needsAnd {
		helperCount++
	}
	if needsOr {
		helperCount++
	}
	if needsXor {
		helperCount++
	}
	if needsShl {
		helperCount++
	}
	if needsShr {
		helperCount++
	}
	if needsClz {
		helperCount++
	}
	if needsCtz {
		helperCount++
	}
	if needsPopcnt {
		helperCount++
	}
	if needsRotl {
		helperCount++
	}
	if needsRotr {
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
	if needsRandom {
		helperCount++
	}
	if needsMalloc {
		helperCount++
	}
	if needsMemcpy {
		helperCount++
	}
	if needsExitEarly {
		helperCount++
	}
	if needsFdCloseEarly {
		helperCount++
	}
	if needsReadEarly {
		helperCount++
	}
	if needsWriteEarly {
		helperCount++
	}
	if needsSeekEarly {
		helperCount++
	}
	if needsSyncEarly {
		helperCount++
	}
	if needsOpenEarly {
		helperCount++
	}
	if needsMkdirEarly {
		helperCount++
	}
	if needsRmdirEarly {
		helperCount++
	}
	if needsUnlinkEarly {
		helperCount++
	}
	if needsRenameEarly {
		helperCount++
	}
	if needsStatEarly {
		helperCount++
	}
	if needsGetArgsSizesEarly {
		helperCount++
	}
	if needsGetArgsEarly {
		helperCount++
	}
	if needsGetEnvironSizesEarly {
		helperCount++
	}
	if needsGetEnvironEarly {
		helperCount++
	}
	if needsArgcEarly {
		helperCount++
	}
	if needsEnvcEarly {
		helperCount++
	}
	if needsStdinReadEarly {
		helperCount++
	}
	if needsStdoutWriteEarly {
		helperCount++
	}
	if needsStderrWriteEarly {
		helperCount++
	}
	if needsFdstatEarly {
		helperCount++
	}
	if needsFstatEarly {
		helperCount++
	}
	if needsReaddirEarly {
		helperCount++
	}
	if needsYieldEarly {
		helperCount++
	}
	if needsRaiseEarly {
		helperCount++
	}
	if needsSetFdFlagsEarly {
		helperCount++
	}
	if needsDatasyncEarly {
		helperCount++
	}
	if needsAllocateEarly {
		helperCount++
	}
	if needsAdviseEarly {
		helperCount++
	}
	if needsLinkEarly {
		helperCount++
	}
	if needsTellEarly {
		helperCount++
	}
	if needsSymlinkEarly {
		helperCount++
	}
	if needsReadlinkEarly {
		helperCount++
	}
	if needsPrestatGetEarly {
		helperCount++
	}
	if needsPrestatDirNameEarly {
		helperCount++
	}
	if needsSockRecvEarly {
		helperCount++
	}
	if needsSockSendEarly {
		helperCount++
	}
	if needsSockShutdownEarly {
		helperCount++
	}
	if needsHTTPGetEarly {
		helperCount++
	}
	if needsHTTPPostEarly {
		helperCount++
	}
	if needsHTTPRequestEarly {
		helperCount++
	}
	if needsTimeEarly {
		helperCount++
	}
	if needsMillisEarly {
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
	if needsMod {
		funcIdx["mod"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsPow {
		funcIdx["pow"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsSqrt {
		funcIdx["sqrt"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsAnd {
		funcIdx["and"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsOr {
		funcIdx["or"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsXor {
		funcIdx["xor"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsShl {
		funcIdx["shl"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsShr {
		funcIdx["shr"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsClz {
		funcIdx["clz"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsCtz {
		funcIdx["ctz"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsPopcnt {
		funcIdx["popcnt"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsRotl {
		funcIdx["rotl"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsRotr {
		funcIdx["rotr"] = len(m.imports) + helperIdx
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
	if needsRandom {
		funcIdx["random"] = len(m.imports) + helperIdx
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
	if needsExitEarly {
		funcIdx["exit"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsFdCloseEarly {
		funcIdx["close"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsReadEarly {
		funcIdx["read"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsWriteEarly {
		funcIdx["write"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsSeekEarly {
		funcIdx["seek"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsSyncEarly {
		funcIdx["sync"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsOpenEarly {
		funcIdx["open"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsMkdirEarly {
		funcIdx["mkdir"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsRmdirEarly {
		funcIdx["rmdir"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsUnlinkEarly {
		funcIdx["unlink"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsRenameEarly {
		funcIdx["rename"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsStatEarly {
		funcIdx["stat"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsGetArgsSizesEarly {
		funcIdx["get_args_sizes"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsGetArgsEarly {
		funcIdx["get_args"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsGetEnvironSizesEarly {
		funcIdx["get_environ_sizes"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsGetEnvironEarly {
		funcIdx["get_environ"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsArgcEarly {
		funcIdx["argc"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsEnvcEarly {
		funcIdx["envc"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsStdinReadEarly {
		funcIdx["stdin_read"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsStdoutWriteEarly {
		funcIdx["stdout_write"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsStderrWriteEarly {
		funcIdx["stderr_write"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsFdstatEarly {
		funcIdx["fdstat"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsFstatEarly {
		funcIdx["fstat"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsReaddirEarly {
		funcIdx["readdir"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsYieldEarly {
		funcIdx["yield"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsRaiseEarly {
		funcIdx["raise"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsSetFdFlagsEarly {
		funcIdx["set_fd_flags"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsDatasyncEarly {
		funcIdx["datasync"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsAllocateEarly {
		funcIdx["allocate"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsAdviseEarly {
		funcIdx["advise"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsLinkEarly {
		funcIdx["link"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsTellEarly {
		funcIdx["tell"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsSymlinkEarly {
		funcIdx["symlink"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsReadlinkEarly {
		funcIdx["readlink"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsPrestatGetEarly {
		funcIdx["prestat_get"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsPrestatDirNameEarly {
		funcIdx["prestat_dir_name"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsSockRecvEarly {
		funcIdx["sock_recv"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsSockSendEarly {
		funcIdx["sock_send"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsSockShutdownEarly {
		funcIdx["sock_shutdown"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsHTTPGetEarly {
		funcIdx["http_get"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsHTTPPostEarly {
		funcIdx["http_post"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsHTTPRequestEarly {
		funcIdx["http_request"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsTimeEarly {
		funcIdx["time"] = len(m.imports) + helperIdx
		helperIdx++
	}
	if needsMillisEarly {
		funcIdx["millis"] = len(m.imports) + helperIdx
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
	if needsMod {
		code := generateModHelper()
		m.AddFunction("mod", 2, code, 0) // 2 params, 0 locals
	}
	if needsPow {
		code := generatePowHelper()
		m.AddFunction("pow", 2, code, 2) // 2 params (base, exp), 2 locals (result, i)
	}
	if needsSqrt {
		code := generateSqrtHelper()
		m.AddFunction("sqrt", 1, code, 2) // 1 param, 2 locals (x, next)
	}
	if needsAnd {
		code := generateAndHelper()
		m.AddFunction("and", 2, code, 0) // 2 params, 0 locals
	}
	if needsOr {
		code := generateOrHelper()
		m.AddFunction("or", 2, code, 0) // 2 params, 0 locals
	}
	if needsXor {
		code := generateXorHelper()
		m.AddFunction("xor", 2, code, 0) // 2 params, 0 locals
	}
	if needsShl {
		code := generateShlHelper()
		m.AddFunction("shl", 2, code, 0) // 2 params, 0 locals
	}
	if needsShr {
		code := generateShrHelper()
		m.AddFunction("shr", 2, code, 0) // 2 params, 0 locals
	}
	if needsClz {
		code := generateClzHelper()
		m.AddFunction("clz", 1, code, 0) // 1 param, 0 locals
	}
	if needsCtz {
		code := generateCtzHelper()
		m.AddFunction("ctz", 1, code, 0) // 1 param, 0 locals
	}
	if needsPopcnt {
		code := generatePopcntHelper()
		m.AddFunction("popcnt", 1, code, 0) // 1 param, 0 locals
	}
	if needsRotl {
		code := generateRotlHelper()
		m.AddFunction("rotl", 2, code, 0) // 2 params, 0 locals
	}
	if needsRotr {
		code := generateRotrHelper()
		m.AddFunction("rotr", 2, code, 0) // 2 params, 0 locals
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
	if needsRandom {
		code := generateRandomHelper(funcIdx["random_get"])
		m.AddFunction("random", 0, code, 0) // 0 params, 0 locals
	}
	if needsMalloc {
		code := generateMallocHelper(heapPtrIdx)
		m.AddFunction("malloc", 1, code, 0) // 1 param (size), 0 locals
	}
	if needsMemcpy {
		code := generateMemcpyHelper()
		m.AddFunction("memcpy", 3, code, 1) // 3 params (dest, src, len), 1 local
	}
	if needsExitEarly {
		code := generateExitHelper(funcIdx["proc_exit"])
		m.AddFunction("exit", 1, code, 0) // 1 param (code), 0 locals
	}
	if needsFdCloseEarly {
		code := generateFdCloseHelper(funcIdx["fd_close"])
		m.AddFunction("close", 1, code, 0) // 1 param (fd), 0 locals
	}
	if needsReadEarly {
		code := generateReadHelper(funcIdx["fd_read"])
		m.AddFunction("read", 3, code, 0) // 3 params (fd, buf, len), 0 locals
	}
	if needsWriteEarly {
		code := generateWriteHelper(funcIdx["fd_write"])
		m.AddFunction("write", 3, code, 0) // 3 params (fd, buf, len), 0 locals
	}
	if needsSeekEarly {
		code := generateSeekHelper(funcIdx["fd_seek"])
		m.AddFunction("seek", 4, code, 0) // 4 params (fd, offset, whence, newoffset_ptr), 0 locals
	}
	if needsSyncEarly {
		code := generateSyncHelper(funcIdx["fd_sync"])
		m.AddFunction("sync", 1, code, 0) // 1 param (fd), 0 locals
	}
	if needsOpenEarly {
		code := generateOpenHelper(funcIdx["path_open"])
		m.AddFunction("open", 8, code, 0) // 8 params (dirfd, path, path_len, oflags, rights_base, rights_inheriting, fdflags, fd_ptr), 0 locals
	}
	if needsMkdirEarly {
		code := generateMkdirHelper(funcIdx["path_create_directory"])
		m.AddFunction("mkdir", 3, code, 0) // 3 params (dirfd, path, path_len), 0 locals
	}
	if needsRmdirEarly {
		code := generateRmdirHelper(funcIdx["path_remove_directory"])
		m.AddFunction("rmdir", 3, code, 0) // 3 params (dirfd, path, path_len), 0 locals
	}
	if needsUnlinkEarly {
		code := generateUnlinkHelper(funcIdx["path_unlink_file"])
		m.AddFunction("unlink", 3, code, 0) // 3 params (dirfd, path, path_len), 0 locals
	}
	if needsRenameEarly {
		code := generateRenameHelper(funcIdx["path_rename"])
		m.AddFunction("rename", 6, code, 0) // 6 params (old_dirfd, old_path, old_path_len, new_dirfd, new_path, new_path_len), 0 locals
	}
	if needsStatEarly {
		code := generateStatHelper(funcIdx["path_filestat_get"])
		m.AddFunction("stat", 5, code, 0) // 5 params (dirfd, flags, path, path_len, buf), 0 locals
	}
	if needsGetArgsSizesEarly {
		code := generateGetArgsSizesHelper(funcIdx["args_sizes_get"])
		m.AddFunction("get_args_sizes", 2, code, 0) // 2 params (argc_ptr, argv_buf_size_ptr), 0 locals
	}
	if needsGetArgsEarly {
		code := generateGetArgsHelper(funcIdx["args_get"])
		m.AddFunction("get_args", 2, code, 0) // 2 params (argv_ptr, argv_buf_ptr), 0 locals
	}
	if needsGetEnvironSizesEarly {
		code := generateGetEnvironSizesHelper(funcIdx["environ_sizes_get"])
		m.AddFunction("get_environ_sizes", 2, code, 0) // 2 params (environc_ptr, environ_buf_size_ptr), 0 locals
	}
	if needsGetEnvironEarly {
		code := generateGetEnvironHelper(funcIdx["environ_get"])
		m.AddFunction("get_environ", 2, code, 0) // 2 params (environ_ptr, environ_buf_ptr), 0 locals
	}
	if needsArgcEarly {
		code := generateArgcHelper(funcIdx["args_sizes_get"])
		m.AddFunction("argc", 0, code, 0) // 0 params, returns argc
	}
	if needsEnvcEarly {
		code := generateEnvcHelper(funcIdx["environ_sizes_get"])
		m.AddFunction("envc", 0, code, 0) // 0 params, returns envc
	}
	if needsStdinReadEarly {
		code := generateStdinReadHelper(funcIdx["fd_read"])
		m.AddFunction("stdin_read", 2, code, 0) // 2 params (buf, len), returns bytes read
	}
	if needsStdoutWriteEarly {
		code := generateStdoutWriteHelper(funcIdx["fd_write"])
		m.AddFunction("stdout_write", 2, code, 0) // 2 params (buf, len), returns bytes written
	}
	if needsStderrWriteEarly {
		code := generateStderrWriteHelper(funcIdx["fd_write"])
		m.AddFunction("stderr_write", 2, code, 0) // 2 params (buf, len), returns bytes written
	}
	if needsFdstatEarly {
		code := generateFdstatHelper(funcIdx["fd_fdstat_get"])
		m.AddFunction("fdstat", 2, code, 0) // 2 params (fd, buf), 0 locals
	}
	if needsFstatEarly {
		code := generateFstatHelper(funcIdx["fd_filestat_get"])
		m.AddFunction("fstat", 2, code, 0) // 2 params (fd, buf), 0 locals
	}
	if needsReaddirEarly {
		code := generateReaddirHelper(funcIdx["fd_readdir"])
		m.AddFunction("readdir", 4, code, 0) // 4 params (fd, buf, buf_len, cookie), 0 locals
	}
	if needsYieldEarly {
		code := generateYieldHelper(funcIdx["sched_yield"])
		m.AddFunction("yield", 0, code, 0) // 0 params, 0 locals
	}
	if needsRaiseEarly {
		code := generateRaiseHelper(funcIdx["proc_raise"])
		m.AddFunction("raise", 1, code, 0) // 1 param (sig), 0 locals
	}
	if needsSetFdFlagsEarly {
		code := generateSetFdFlagsHelper(funcIdx["fd_fdstat_set_flags"])
		m.AddFunction("set_fd_flags", 2, code, 0) // 2 params (fd, flags), 0 locals
	}
	if needsDatasyncEarly {
		code := generateDatasyncHelper(funcIdx["fd_datasync"])
		m.AddFunction("datasync", 1, code, 0) // 1 param (fd), 0 locals
	}
	if needsAllocateEarly {
		code := generateAllocateHelper(funcIdx["fd_allocate"])
		m.AddFunction("allocate", 3, code, 0) // 3 params (fd, offset, len), 0 locals
	}
	if needsAdviseEarly {
		code := generateAdviseHelper(funcIdx["fd_advise"])
		m.AddFunction("advise", 4, code, 0) // 4 params (fd, offset, len, advice), 0 locals
	}
	if needsLinkEarly {
		code := generateLinkHelper(funcIdx["path_link"])
		m.AddFunction("link", 7, code, 0) // 7 params (old_fd, old_flags, old_path, old_path_len, new_fd, new_path, new_path_len), 0 locals
	}
	if needsTellEarly {
		code := generateTellHelper(funcIdx["fd_tell"])
		m.AddFunction("tell", 2, code, 0) // 2 params (fd, offset_ptr), 0 locals
	}
	if needsSymlinkEarly {
		code := generateSymlinkHelper(funcIdx["path_symlink"])
		m.AddFunction("symlink", 5, code, 0) // 5 params (old_path, old_path_len, fd, new_path, new_path_len), 0 locals
	}
	if needsReadlinkEarly {
		code := generateReadlinkHelper(funcIdx["path_readlink"])
		m.AddFunction("readlink", 6, code, 0) // 6 params (fd, path, path_len, buf, buf_len, bufused_ptr), 0 locals
	}
	if needsPrestatGetEarly {
		code := generatePrestatGetHelper(funcIdx["fd_prestat_get"])
		m.AddFunction("prestat_get", 2, code, 0) // 2 params (fd, buf), 0 locals
	}
	if needsPrestatDirNameEarly {
		code := generatePrestatDirNameHelper(funcIdx["fd_prestat_dir_name"])
		m.AddFunction("prestat_dir_name", 3, code, 0) // 3 params (fd, path, path_len), 0 locals
	}
	if needsSockRecvEarly {
		code := generateSockRecvHelper(funcIdx["sock_recv"])
		m.AddFunction("sock_recv", 6, code, 0) // 6 params (fd, ri_data, ri_data_len, ri_flags, ro_datalen, ro_flags), 0 locals
	}
	if needsSockSendEarly {
		code := generateSockSendHelper(funcIdx["sock_send"])
		m.AddFunction("sock_send", 5, code, 0) // 5 params (fd, si_data, si_data_len, si_flags, so_datalen), 0 locals
	}
	if needsSockShutdownEarly {
		code := generateSockShutdownHelper(funcIdx["sock_shutdown"])
		m.AddFunction("sock_shutdown", 2, code, 0) // 2 params (fd, how), 0 locals
	}
	if needsHTTPGetEarly {
		code := generateHTTPGetHelper()
		m.AddFunction("http_get", 3, code, 0) // 3 params (url_ptr, url_len, response_ptr), 0 locals
	}
	if needsHTTPPostEarly {
		code := generateHTTPPostHelper()
		m.AddFunction("http_post", 5, code, 0) // 5 params (url_ptr, url_len, body_ptr, body_len, response_ptr), 0 locals
	}
	if needsHTTPRequestEarly {
		code := generateHTTPRequestHelper()
		m.AddFunction("http_request", 8, code, 0) // 8 params (method, url_ptr, url_len, headers_ptr, headers_len, body_ptr, body_len, response_ptr), 0 locals
	}
	if needsTimeEarly {
		code := generateTimeHelper(funcIdx["clock_time_get"])
		m.AddFunction("time", 0, code, 0) // 0 params, 0 locals, returns i32 (seconds)
	}
	if needsMillisEarly {
		code := generateMillisHelper(funcIdx["clock_time_get"])
		m.AddFunction("millis", 0, code, 0) // 0 params, 0 locals, returns i32 (milliseconds)
	}

	// Add async helper functions if needed
	if hasAsyncFunctions {
		// Add async runtime initialization
		code := generateAsyncRuntimeInitHelper()
		m.AddFunction("_async_init", 0, code, 0) // 0 params, 0 locals, returns i32
		funcIdx["_async_init"] = len(m.imports) + helperIdx
		helperIdx++

		// Add async yield/resume helpers
		code = generateAsyncYieldHelper()
		m.AddFunction("_async_yield", 0, code, 0) // 0 params, 0 locals, returns i32
		funcIdx["_async_yield"] = len(m.imports) + helperIdx
		helperIdx++

		code = generateAsyncResumeHelper()
		m.AddFunction("_async_resume", 1, code, 0) // 1 param (continuation), 0 locals, returns i32
		funcIdx["_async_resume"] = len(m.imports) + helperIdx
		helperIdx++

		// Add async_sleep helper
		code = generateAsyncSleepHelper()
		m.AddFunction("async_sleep", 1, code, 0) // 1 param (ms), 0 locals, returns i32
		funcIdx["async_sleep"] = len(m.imports) + helperIdx
		helperIdx++

		// Add async_read helper if fd_read is available
		if _, ok := funcIdx["fd_read"]; ok {
			code = generateAsyncReadHelper(funcIdx["fd_read"])
			m.AddFunction("async_read", 3, code, 0) // 3 params (fd, buf, len), 0 locals, returns i32
			funcIdx["async_read"] = len(m.imports) + helperIdx
			helperIdx++
		}
	}

	for _, fn := range file.Fns {
		code, numLocals := Compile(fn, funcIdx, globalIdx, structs, strings, hasAsyncFunctions)
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
	case *parser.IndexAssignStmt:
		return usesPrintIntExpr(s.Array) || usesPrintIntExpr(s.Index) || usesPrintIntExpr(s.Value)
	case *parser.FieldAssignStmt:
		return usesPrintIntExpr(s.Expr) || usesPrintIntExpr(s.Value)
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
	case *parser.UnaryExpr:
		return usesPrintIntExpr(e.Expr)
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
	case *parser.ReturnExpr:
		return usesPrintIntExpr(e.Value)
	case *parser.IndexExpr:
		return usesPrintIntExpr(e.Array) || usesPrintIntExpr(e.Index)
	case *parser.ArrayLit:
		for _, elem := range e.Elements {
			if usesPrintIntExpr(elem) {
				return true
			}
		}
	case *parser.StructLit:
		for _, f := range e.Fields {
			if usesPrintIntExpr(f.Value) {
				return true
			}
		}
	case *parser.FieldExpr:
		return usesPrintIntExpr(e.Expr)
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
	case *parser.IndexAssignStmt:
		return usesPrintlnExpr(s.Array) || usesPrintlnExpr(s.Index) || usesPrintlnExpr(s.Value)
	case *parser.FieldAssignStmt:
		return usesPrintlnExpr(s.Expr) || usesPrintlnExpr(s.Value)
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
	case *parser.UnaryExpr:
		return usesPrintlnExpr(e.Expr)
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
	case *parser.ReturnExpr:
		return usesPrintlnExpr(e.Value)
	case *parser.IndexExpr:
		return usesPrintlnExpr(e.Array) || usesPrintlnExpr(e.Index)
	case *parser.ArrayLit:
		for _, elem := range e.Elements {
			if usesPrintlnExpr(elem) {
				return true
			}
		}
	case *parser.StructLit:
		for _, f := range e.Fields {
			if usesPrintlnExpr(f.Value) {
				return true
			}
		}
	case *parser.FieldExpr:
		return usesPrintlnExpr(e.Expr)
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
	case *parser.FieldAssignStmt:
		return usesBuiltinExpr(s.Expr, name) || usesBuiltinExpr(s.Value, name)
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
	case *parser.StructLit:
		for _, f := range e.Fields {
			if usesBuiltinExpr(f.Value, name) {
				return true
			}
		}
	case *parser.FieldExpr:
		return usesBuiltinExpr(e.Expr, name)
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
	case *parser.IndexAssignStmt:
		collectCallsExpr(s.Array, calls)
		collectCallsExpr(s.Index, calls)
		collectCallsExpr(s.Value, calls)
	case *parser.FieldAssignStmt:
		collectCallsExpr(s.Expr, calls)
		collectCallsExpr(s.Value, calls)
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
	case *parser.UnaryExpr:
		collectCallsExpr(e.Expr, calls)
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
	case *parser.ReturnExpr:
		if e.Value != nil {
			collectCallsExpr(e.Value, calls)
		}
	case *parser.IndexExpr:
		collectCallsExpr(e.Array, calls)
		collectCallsExpr(e.Index, calls)
	case *parser.ArrayLit:
		for _, elem := range e.Elements {
			collectCallsExpr(elem, calls)
		}
	case *parser.StructLit:
		for _, f := range e.Fields {
			collectCallsExpr(f.Value, calls)
		}
	case *parser.FieldExpr:
		collectCallsExpr(e.Expr, calls)
	}
}

func Compile(fn *parser.FnDecl, funcIdx, globalIdx map[string]int, structs map[string]*StructInfo, strings *StringTable, hasAsyncFunctions bool) (code []byte, numLocals int) {
	// Check if async runtime is needed for this function
	asyncRuntimeNeeded := hasAsyncFunctions && fn.Async
	
	c := &Compiler{
		fn:           fn,
		locals:       make(map[string]int),
		localStructs: make(map[string]string),
		funcIdx:      funcIdx,
		globalIdx:    globalIdx,
		structs:      structs,
		strings:      strings,
		isAsync:      fn.Async,
		continuation: "",
		asyncRuntime: asyncRuntimeNeeded,
	}

	// Map params to local indices
	for i, p := range fn.Params {
		c.locals[p.Name] = i
		// Track struct type for parameters
		if structs != nil {
			if _, ok := structs[p.Type]; ok {
				c.localStructs[p.Name] = p.Type
			}
		}
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
		// Track struct type if assigning a struct literal
		if lit, ok := s.Value.(*parser.StructLit); ok {
			c.localStructs[s.Name] = lit.Name
		} else if s.Type != "" {
			// Also track if type annotation is a struct
			if _, ok := c.structs[s.Type]; ok {
				c.localStructs[s.Name] = s.Type
			}
		}
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
	case *parser.FieldAssignStmt:
		// Store value at struct.field
		var code []byte
		// Get the struct address
		code = append(code, c.compileExpr(s.Expr)...)
		// Get struct type from expression
		var structName string
		if ident, ok := s.Expr.(*parser.Ident); ok {
			structName = c.localStructs[ident.Name]
		}
		// Add field offset
		if info, ok := c.structs[structName]; ok {
			if offset, ok := info.Offsets[s.Field]; ok {
				if offset > 0 {
					code = append(code, 0x41) // i32.const
					code = append(code, sleb128(int64(offset))...)
					code = append(code, OpI32Add)
				}
			}
		}
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
		// Builtins that don't produce values
		switch e.Name {
		case "drop", "store", "assert", "panic", "exit":
			return false
		}
		return true
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
	case *parser.StructLit:
		// Allocate space for struct on heap and initialize fields
		info, ok := c.structs[e.Name]
		if !ok {
			return nil // undefined struct
		}
		var code []byte
		// Get current heap pointer (this will be struct address)
		code = append(code, OpGlobalGet, byte(c.globalIdx["__heap_ptr"]))
		// Store the address in a local for later use (we'll return it)
		// Use local.tee to keep value on stack while storing
		localIdx := c.numLocals
		c.numLocals++
		code = append(code, OpLocalTee, byte(localIdx))
		// Bump heap pointer by struct size
		code = append(code, 0x41) // i32.const
		code = append(code, sleb128(int64(info.Size))...)
		code = append(code, OpI32Add)
		code = append(code, OpGlobalSet, byte(c.globalIdx["__heap_ptr"]))
		// Initialize fields
		for _, field := range e.Fields {
			offset, ok := info.Offsets[field.Name]
			if !ok {
				continue
			}
			// Get struct base address
			code = append(code, OpLocalGet, byte(localIdx))
			// Add field offset
			if offset > 0 {
				code = append(code, 0x41) // i32.const
				code = append(code, sleb128(int64(offset))...)
				code = append(code, OpI32Add)
			}
			// Compute field value
			code = append(code, c.compileExpr(field.Value)...)
			// Store the value
			code = append(code, OpI32Store, 2, 0)
		}
		// Return struct address
		code = append(code, OpLocalGet, byte(localIdx))
		return code
	case *parser.FieldExpr:
		// Load field from struct
		var code []byte
		code = append(code, c.compileExpr(e.Expr)...)
		// Get struct type from expression
		var structName string
		if ident, ok := e.Expr.(*parser.Ident); ok {
			structName = c.localStructs[ident.Name]
		}
		if info, ok := c.structs[structName]; ok {
			if offset, ok := info.Offsets[e.Field]; ok {
				if offset > 0 {
					code = append(code, 0x41) // i32.const
					code = append(code, sleb128(int64(offset))...)
					code = append(code, OpI32Add)
				}
			}
		}
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
		case "panic":
			// panic(msg) - print message to stderr and exit with code 1
			// msg is a string (pointer to data)
			code = nil
			// Get string address from argument
			code = append(code, c.compileExpr(e.Args[0])...)
			// Store string ptr at iovec.buf (address 0)
			code = append(code, 0x41, 0) // i32.const 0 (iovec address)
			code = append(code, c.compileExpr(e.Args[0])...)
			code = append(code, OpI32Store, 2, 0)
			// Store string length at iovec.len (address 4)
			// For now, use a fixed length of 32 or compute from string table
			code = append(code, 0x41, 4) // i32.const 4
			code = append(code, 0x41, 32) // i32.const 32 (max len)
			code = append(code, OpI32Store, 2, 0)
			// fd_write(fd=2, iovs=0, iovs_len=1, nwritten=8)
			code = append(code, 0x41, 2) // fd = 2 (stderr)
			code = append(code, 0x41, 0) // iovs = 0
			code = append(code, 0x41, 1) // iovs_len = 1
			code = append(code, 0x41, 8) // nwritten = 8
			if idx, ok := c.funcIdx["fd_write"]; ok {
				code = append(code, OpCall, byte(idx))
				code = append(code, 0x1a) // drop the result
			}
			// Exit with code 1
			code = append(code, 0x41, 1) // i32.const 1
			if idx, ok := c.funcIdx["proc_exit"]; ok {
				code = append(code, OpCall, byte(idx))
			}
			code = append(code, 0x00) // unreachable
		case "assert":
			// assert(condition) - if condition is 0, exit with code 1
			code = nil
			code = append(code, c.compileExpr(e.Args[0])...)
			code = append(code, OpI32Eqz) // check if condition == 0
			code = append(code, OpIf, 0x40) // if (void block)
			// Exit with code 1 on assertion failure
			code = append(code, 0x41, 1) // i32.const 1
			if idx, ok := c.funcIdx["proc_exit"]; ok {
				code = append(code, OpCall, byte(idx))
			}
			code = append(code, 0x00) // unreachable
			code = append(code, OpEnd)
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
		case "sign":
			// sign(n) - returns -1, 0, or 1
			code = nil
			n := c.compileExpr(e.Args[0])
			// if n < 0 { -1 } else if n > 0 { 1 } else { 0 }
			code = append(code, n...)
			code = append(code, 0x41, 0) // i32.const 0
			code = append(code, OpI32LtS)
			code = append(code, OpIf, 0x7f) // if n < 0
			code = append(code, 0x41, 0x7f) // i32.const -1
			code = append(code, OpElse)
			code = append(code, n...)
			code = append(code, 0x41, 0) // i32.const 0
			code = append(code, OpI32GtS)
			code = append(code, OpIf, 0x7f) // if n > 0
			code = append(code, 0x41, 1) // i32.const 1
			code = append(code, OpElse)
			code = append(code, 0x41, 0) // i32.const 0
			code = append(code, OpEnd)
			code = append(code, OpEnd)
		case "negate":
			// negate(n) - returns -n
			code = nil
			code = append(code, 0x41, 0) // i32.const 0
			code = append(code, c.compileExpr(e.Args[0])...)
			code = append(code, OpI32Sub) // 0 - n
		case "clamp":
			// clamp(n, lo, hi) - clamp value to range [lo, hi]
			code = nil
			n := c.compileExpr(e.Args[0])
			lo := c.compileExpr(e.Args[1])
			hi := c.compileExpr(e.Args[2])
			// if n < lo { lo } else if n > hi { hi } else { n }
			code = append(code, n...)
			code = append(code, lo...)
			code = append(code, OpI32LtS)
			code = append(code, OpIf, 0x7f) // if n < lo
			code = append(code, lo...)
			code = append(code, OpElse)
			code = append(code, n...)
			code = append(code, hi...)
			code = append(code, OpI32GtS)
			code = append(code, OpIf, 0x7f) // if n > hi
			code = append(code, hi...)
			code = append(code, OpElse)
			code = append(code, n...)
			code = append(code, OpEnd)
			code = append(code, OpEnd)
		case "is_even":
			// is_even(n) - returns 1 if n is even, 0 otherwise
			code = nil
			code = append(code, c.compileExpr(e.Args[0])...)
			code = append(code, 0x41, 1)    // i32.const 1
			code = append(code, OpI32And)   // n & 1
			code = append(code, OpI32Eqz)   // == 0 (is even)
		case "is_odd":
			// is_odd(n) - returns 1 if n is odd, 0 otherwise
			code = nil
			code = append(code, c.compileExpr(e.Args[0])...)
			code = append(code, 0x41, 1)    // i32.const 1
			code = append(code, OpI32And)   // n & 1 (is odd)
		case "square":
			// square(n) - returns n * n
			code = nil
			n := c.compileExpr(e.Args[0])
			code = append(code, n...)
			code = append(code, n...)
			code = append(code, OpI32Mul)
		case "cube":
			// cube(n) - returns n * n * n
			code = nil
			n := c.compileExpr(e.Args[0])
			code = append(code, n...)
			code = append(code, n...)
			code = append(code, OpI32Mul)
			code = append(code, n...)
			code = append(code, OpI32Mul)
		case "log2":
			// log2(n) - returns floor(log2(n)) for n > 0
			// calculated as 31 - clz(n)
			code = nil
			code = append(code, 0x41, 31)   // i32.const 31
			code = append(code, c.compileExpr(e.Args[0])...)
			code = append(code, OpI32Clz)   // clz(n)
			code = append(code, OpI32Sub)   // 31 - clz(n)
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
		// ============================================================================
		// LIST OPERATIONS
		// ============================================================================
		case "list_new":
			// Allocate a new list with initial capacity of 8
			// Layout: [capacity:i32][length:i32][elements:i32...]
			// Initial size: 8 (header) + 32 (8 elements * 4 bytes) = 40 bytes
			code = nil
			heapIdx := c.globalIdx["__heap_ptr"]
			// Get current heap pointer (this will be the list address)
			code = append(code, OpGlobalGet, byte(heapIdx))
			// Store capacity (8) at offset 0
			code = append(code, OpGlobalGet, byte(heapIdx))
			code = append(code, 0x41, 8) // i32.const 8
			code = append(code, OpI32Store, 2, 0)
			// Store length (0) at offset 4
			code = append(code, OpGlobalGet, byte(heapIdx))
			code = append(code, 0x41, 4) // i32.const 4
			code = append(code, OpI32Add)
			code = append(code, 0x41, 0) // i32.const 0
			code = append(code, OpI32Store, 2, 0)
			// Update heap pointer: heap_ptr += 40
			code = append(code, OpGlobalGet, byte(heapIdx))
			code = append(code, 0x41, 40) // i32.const 40
			code = append(code, OpI32Add)
			code = append(code, OpGlobalSet, byte(heapIdx))

		case "list_len":
			// list_len(list) - return length at offset 4
			code = nil
			code = append(code, c.compileExpr(e.Args[0])...)
			code = append(code, 0x41, 4) // i32.const 4
			code = append(code, OpI32Add)
			code = append(code, OpI32Load, 2, 0)

		case "list_get":
			// list_get(list, index) - return element at offset 8 + index*4
			code = nil
			list := c.compileExpr(e.Args[0])
			index := c.compileExpr(e.Args[1])
			code = append(code, list...)
			code = append(code, 0x41, 8) // i32.const 8
			code = append(code, OpI32Add)
			code = append(code, index...)
			code = append(code, 0x41, 4) // i32.const 4
			code = append(code, OpI32Mul)
			code = append(code, OpI32Add)
			code = append(code, OpI32Load, 2, 0)

		case "list_set":
			// list_set(list, index, value) - store value at offset 8 + index*4
			code = nil
			list := c.compileExpr(e.Args[0])
			index := c.compileExpr(e.Args[1])
			value := c.compileExpr(e.Args[2])
			// Calculate address: list + 8 + index*4
			code = append(code, list...)
			code = append(code, 0x41, 8) // i32.const 8
			code = append(code, OpI32Add)
			code = append(code, index...)
			code = append(code, 0x41, 4) // i32.const 4
			code = append(code, OpI32Mul)
			code = append(code, OpI32Add)
			// Store the value
			code = append(code, value...)
			code = append(code, OpI32Store, 2, 0)
			// Return 0 (void-like)
			code = append(code, 0x41, 0)

		case "list_push":
			// list_push(list, value) - append value to list
			// For simplicity, assume we don't exceed capacity (no dynamic growth)
			code = nil
			list := c.compileExpr(e.Args[0])
			value := c.compileExpr(e.Args[1])

			// Calculate store address: list + 8 + length*4
			code = append(code, list...)
			code = append(code, 0x41, 8) // i32.const 8
			code = append(code, OpI32Add)
			// Load current length
			code = append(code, list...)
			code = append(code, 0x41, 4) // i32.const 4
			code = append(code, OpI32Add)
			code = append(code, OpI32Load, 2, 0)
			// Multiply by 4
			code = append(code, 0x41, 4)
			code = append(code, OpI32Mul)
			code = append(code, OpI32Add)
			// Store value at calculated address
			code = append(code, value...)
			code = append(code, OpI32Store, 2, 0)
			// Increment length: store length+1 at list+4
			code = append(code, list...)
			code = append(code, 0x41, 4)
			code = append(code, OpI32Add)
			// Load old length
			code = append(code, list...)
			code = append(code, 0x41, 4)
			code = append(code, OpI32Add)
			code = append(code, OpI32Load, 2, 0)
			// Add 1
			code = append(code, 0x41, 1)
			code = append(code, OpI32Add)
			// Store new length
			code = append(code, OpI32Store, 2, 0)
			// Return 0 (void-like)
			code = append(code, 0x41, 0)

		case "list_pop":
			// list_pop(list) - remove and return last element
			code = nil
			list := c.compileExpr(e.Args[0])
			// Decrement length first
			code = append(code, list...)
			code = append(code, 0x41, 4)
			code = append(code, OpI32Add)
			// Load old length, subtract 1, store
			code = append(code, list...)
			code = append(code, 0x41, 4)
			code = append(code, OpI32Add)
			code = append(code, OpI32Load, 2, 0)
			code = append(code, 0x41, 1)
			code = append(code, OpI32Sub)
			code = append(code, OpI32Store, 2, 0)
			// Load element at new length position (list + 8 + new_length*4)
			code = append(code, list...)
			code = append(code, 0x41, 8)
			code = append(code, OpI32Add)
			// Load new length
			code = append(code, list...)
			code = append(code, 0x41, 4)
			code = append(code, OpI32Add)
			code = append(code, OpI32Load, 2, 0)
			// Multiply by 4
			code = append(code, 0x41, 4)
			code = append(code, OpI32Mul)
			code = append(code, OpI32Add)
			code = append(code, OpI32Load, 2, 0)

		// ============================================================================
		// MAP OPERATIONS
		// ============================================================================
		case "map_new":
			// Allocate a new map with initial capacity of 8 entries
			// Layout: [capacity:i32][count:i32][entries:(key:i32,value:i32)...]
			// Entry key of -1 means empty slot
			// Initial size: 8 (header) + 64 (8 entries * 8 bytes) = 72 bytes
			code = nil
			heapIdx := c.globalIdx["__heap_ptr"]
			// Get current heap pointer (this will be the map address)
			code = append(code, OpGlobalGet, byte(heapIdx))
			// Store capacity (8) at offset 0
			code = append(code, OpGlobalGet, byte(heapIdx))
			code = append(code, 0x41, 8)
			code = append(code, OpI32Store, 2, 0)
			// Store count (0) at offset 4
			code = append(code, OpGlobalGet, byte(heapIdx))
			code = append(code, 0x41, 4)
			code = append(code, OpI32Add)
			code = append(code, 0x41, 0)
			code = append(code, OpI32Store, 2, 0)
			// Initialize all 8 entry keys to -1 (empty)
			for i := 0; i < 8; i++ {
				code = append(code, OpGlobalGet, byte(heapIdx))
				code = append(code, 0x41)
				code = append(code, sleb128(int64(8+i*8))...) // offset for key
				code = append(code, OpI32Add)
				code = append(code, 0x41)
				code = append(code, sleb128(-1)...) // -1 = empty
				code = append(code, OpI32Store, 2, 0)
			}
			// Update heap pointer: heap_ptr += 72
			code = append(code, OpGlobalGet, byte(heapIdx))
			code = append(code, 0x41, 72)
			code = append(code, OpI32Add)
			code = append(code, OpGlobalSet, byte(heapIdx))

		case "map_set":
			// map_set(map, key, value) - set key-value pair
			// Simple hash: key % capacity (8)
			// Entry offset = 8 + (key % 8) * 8
			code = nil
			mapPtr := c.compileExpr(e.Args[0])
			key := c.compileExpr(e.Args[1])
			value := c.compileExpr(e.Args[2])

			// Calculate entry address: map + 8 + (key % 8) * 8
			code = append(code, mapPtr...)
			code = append(code, 0x41, 8)
			code = append(code, OpI32Add)
			code = append(code, key...)
			code = append(code, 0x41, 8)
			code = append(code, 0x6f) // i32.rem_s
			code = append(code, 0x41, 8)
			code = append(code, OpI32Mul)
			code = append(code, OpI32Add)
			// Store key at this address
			code = append(code, key...)
			code = append(code, OpI32Store, 2, 0)
			// Store value at address + 4
			code = append(code, mapPtr...)
			code = append(code, 0x41, 8)
			code = append(code, OpI32Add)
			code = append(code, key...)
			code = append(code, 0x41, 8)
			code = append(code, 0x6f) // i32.rem_s
			code = append(code, 0x41, 8)
			code = append(code, OpI32Mul)
			code = append(code, OpI32Add)
			code = append(code, 0x41, 4)
			code = append(code, OpI32Add)
			code = append(code, value...)
			code = append(code, OpI32Store, 2, 0)
			// Return 0
			code = append(code, 0x41, 0)

		case "map_get":
			// map_get(map, key) - get value for key
			code = nil
			mapPtr := c.compileExpr(e.Args[0])
			key := c.compileExpr(e.Args[1])
			// Calculate entry address: map + 8 + (key % 8) * 8 + 4 (for value)
			code = append(code, mapPtr...)
			code = append(code, 0x41, 8)
			code = append(code, OpI32Add)
			code = append(code, key...)
			code = append(code, 0x41, 8)
			code = append(code, 0x6f) // i32.rem_s
			code = append(code, 0x41, 8)
			code = append(code, OpI32Mul)
			code = append(code, OpI32Add)
			code = append(code, 0x41, 4)
			code = append(code, OpI32Add)
			code = append(code, OpI32Load, 2, 0)

		case "map_has":
			// map_has(map, key) - check if key exists (returns 1 or 0)
			code = nil
			mapPtr := c.compileExpr(e.Args[0])
			key := c.compileExpr(e.Args[1])
			// Load key at entry and compare with searched key
			// Entry address: map + 8 + (key % 8) * 8
			code = append(code, mapPtr...)
			code = append(code, 0x41, 8)
			code = append(code, OpI32Add)
			code = append(code, key...)
			code = append(code, 0x41, 8)
			code = append(code, 0x6f) // i32.rem_s
			code = append(code, 0x41, 8)
			code = append(code, OpI32Mul)
			code = append(code, OpI32Add)
			code = append(code, OpI32Load, 2, 0)
			// Compare with key
			code = append(code, key...)
			code = append(code, OpI32Eq)

		// ============================================================================
		// SET OPERATIONS
		// ============================================================================
		case "set_new":
			// Allocate a new set with initial capacity of 8
			// Layout: [capacity:i32][count:i32][values:i32...]
			// Value of -2147483648 (MIN_INT) means empty slot
			// Initial size: 8 (header) + 32 (8 values * 4 bytes) = 40 bytes
			code = nil
			heapIdx := c.globalIdx["__heap_ptr"]
			// Get current heap pointer (this will be the set address)
			code = append(code, OpGlobalGet, byte(heapIdx))
			// Store capacity (8) at offset 0
			code = append(code, OpGlobalGet, byte(heapIdx))
			code = append(code, 0x41, 8)
			code = append(code, OpI32Store, 2, 0)
			// Store count (0) at offset 4
			code = append(code, OpGlobalGet, byte(heapIdx))
			code = append(code, 0x41, 4)
			code = append(code, OpI32Add)
			code = append(code, 0x41, 0)
			code = append(code, OpI32Store, 2, 0)
			// Initialize all 8 slots to MIN_INT (empty)
			for i := 0; i < 8; i++ {
				code = append(code, OpGlobalGet, byte(heapIdx))
				code = append(code, 0x41)
				code = append(code, sleb128(int64(8+i*4))...)
				code = append(code, OpI32Add)
				code = append(code, 0x41)
				code = append(code, sleb128(-2147483648)...) // MIN_INT = empty
				code = append(code, OpI32Store, 2, 0)
			}
			// Update heap pointer: heap_ptr += 40
			code = append(code, OpGlobalGet, byte(heapIdx))
			code = append(code, 0x41, 40)
			code = append(code, OpI32Add)
			code = append(code, OpGlobalSet, byte(heapIdx))

		case "set_add":
			// set_add(set, value) - add value to set (if not already present)
			code = nil
			setPtr := c.compileExpr(e.Args[0])
			value := c.compileExpr(e.Args[1])
			// Simple hash: value % 8
			// Slot address = set + 8 + (value % 8) * 4
			code = append(code, setPtr...)
			code = append(code, 0x41, 8)
			code = append(code, OpI32Add)
			code = append(code, value...)
			code = append(code, 0x41, 8)
			code = append(code, 0x6f) // i32.rem_s
			code = append(code, 0x41, 4)
			code = append(code, OpI32Mul)
			code = append(code, OpI32Add)
			// Store value
			code = append(code, value...)
			code = append(code, OpI32Store, 2, 0)
			// Return 0
			code = append(code, 0x41, 0)

		case "set_has":
			// set_has(set, value) - check if value exists (returns 1 or 0)
			code = nil
			setPtr := c.compileExpr(e.Args[0])
			value := c.compileExpr(e.Args[1])
			// Slot address = set + 8 + (value % 8) * 4
			code = append(code, setPtr...)
			code = append(code, 0x41, 8)
			code = append(code, OpI32Add)
			code = append(code, value...)
			code = append(code, 0x41, 8)
			code = append(code, 0x6f) // i32.rem_s
			code = append(code, 0x41, 4)
			code = append(code, OpI32Mul)
			code = append(code, OpI32Add)
			code = append(code, OpI32Load, 2, 0)
			// Compare with value
			code = append(code, value...)
			code = append(code, OpI32Eq)

		case "set_remove":
			// set_remove(set, value) - remove value from set
			code = nil
			setPtr := c.compileExpr(e.Args[0])
			value := c.compileExpr(e.Args[1])
			// Slot address = set + 8 + (value % 8) * 4
			code = append(code, setPtr...)
			code = append(code, 0x41, 8)
			code = append(code, OpI32Add)
			code = append(code, value...)
			code = append(code, 0x41, 8)
			code = append(code, 0x6f) // i32.rem_s
			code = append(code, 0x41, 4)
			code = append(code, OpI32Mul)
			code = append(code, OpI32Add)
			// Store MIN_INT (empty marker)
			code = append(code, 0x41)
			code = append(code, sleb128(-2147483648)...)
			code = append(code, OpI32Store, 2, 0)
			// Return 0
			code = append(code, 0x41, 0)

		case "set_len":
			// set_len(set) - count non-empty slots
			code = nil
			setPtr := c.compileExpr(e.Args[0])
			// Check each slot and sum up non-MIN_INT values
			for i := 0; i < 8; i++ {
				code = append(code, setPtr...)
				code = append(code, 0x41)
				code = append(code, sleb128(int64(8+i*4))...)
				code = append(code, OpI32Add)
				code = append(code, OpI32Load, 2, 0)
				code = append(code, 0x41)
				code = append(code, sleb128(-2147483648)...)
				code = append(code, OpI32Ne) // 1 if not empty, 0 if empty
				if i > 0 {
					code = append(code, OpI32Add)
				}
			}

		case "async_sleep":
			// async_sleep(ms) - async sleep for milliseconds
			idx := c.funcIdx["async_sleep"]
			code = append(code, OpCall, byte(idx))

		case "async_read":
			// async_read(fd, buf, len) - async file read
			idx := c.funcIdx["async_read"]
			code = append(code, OpCall, byte(idx))

		case "_async_init":
			// Initialize async runtime
			idx := c.funcIdx["_async_init"]
			code = append(code, OpCall, byte(idx))

		case "_async_yield":
			// Yield execution (suspend async function)
			idx := c.funcIdx["_async_yield"]
			code = append(code, OpCall, byte(idx))

		case "_async_resume":
			// Resume suspended async function
			idx := c.funcIdx["_async_resume"]
			code = append(code, OpCall, byte(idx))

		default:
			idx := c.funcIdx[e.Name]
			code = append(code, OpCall, byte(idx))
		}
		return code
	case *parser.AwaitExpr:
		// Await expressions can only be used in async functions
		if !c.isAsync {
			panic("await can only be used in async functions")
		}
		
		// Enhanced await implementation using async runtime
		var code []byte
		
		// Compile the expression being awaited (should be an async operation)
		code = append(code, c.compileExpr(e.Expr)...)
		
		// Check if the operation is ready (for now, assume it's always ready)
		// In a real implementation, this would use polling
		if c.asyncRuntime {
			// Call async_yield to suspend execution
			// This would save state and return to caller
			if idx, ok := c.funcIdx["_async_yield"]; ok {
				code = append(code, OpCall, byte(idx))
				code = append(code, OpDrop) // Drop the result for now
			}
			
			// Store the result in a local variable
			// In a real implementation, this would be handled by the runtime
			localIdx := c.numLocals
			c.numLocals++
			code = append(code, OpLocalTee, byte(localIdx))
			
			// Return from the async function with "not ready" status
			code = append(code, 0x41, 1) // i32.const 1 (not ready)
			code = append(code, 0x0f)    // return
			
			// Continuation point (this would be jumped to when resumed)
			code = append(code, OpBlock, 0x7f) // block with i32 result
			code = append(code, OpLocalGet, byte(localIdx)) // get the stored result
			code = append(code, OpEnd)
		} else {
			// Fallback: just return the expression result (synchronous)
			// This maintains backward compatibility
		}
		
		return code
	}
	return nil
}
