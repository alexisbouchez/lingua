package codegen

import "github.com/alexisbouchez/lingua/parser"

// WASM opcodes
const (
	OpBlock    = 0x02
	OpLoop     = 0x03
	OpIf       = 0x04
	OpElse     = 0x05
	OpEnd      = 0x0b
	OpBr       = 0x0c
	OpBrIf     = 0x0d
	OpCall     = 0x10
	OpLocalGet = 0x20
	OpLocalSet = 0x21
	OpI32Eqz   = 0x45
	OpI32Eq    = 0x46
	OpI32Ne    = 0x47
	OpI32LtS   = 0x48
	OpI32GtS   = 0x4a
	OpI32LeS   = 0x4c
	OpI32GeS   = 0x4e
	OpI32Add   = 0x6a
	OpI32Sub   = 0x6b
	OpI32Mul   = 0x6c
	OpI32Div   = 0x6d

	OpI32Load  = 0x28
	OpI32Store = 0x36
)

type Compiler struct {
	fn        *parser.FnDecl
	locals    map[string]int
	numLocals int
	funcIdx   map[string]int
	strings   *StringTable
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

func CompileFile(file *parser.File, m *Module) {
	// First pass: collect all function calls to detect WASI imports
	wasiImports := map[string]int{
		"fd_write":    4,
		"fd_read":     4,
		"args_get":    2,
		"args_sizes_get": 2,
		"proc_exit":   1,
	}

	usedImports := make(map[string]bool)
	for _, fn := range file.Fns {
		collectCalls(fn.Body, usedImports)
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

	for _, fn := range file.Fns {
		code, numLocals := Compile(fn, funcIdx, strings)
		m.AddFunction(fn.Name, len(fn.Params), code, numLocals)
	}

	// Add string data to module
	if len(strings.Bytes()) > 0 {
		m.AddData(strings.offset, strings.Bytes())
	}
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
		if e.Name == "print_str" {
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

func Compile(fn *parser.FnDecl, funcIdx map[string]int, strings *StringTable) (code []byte, numLocals int) {
	c := &Compiler{
		fn:      fn,
		locals:  make(map[string]int),
		funcIdx: funcIdx,
		strings: strings,
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
		idx := c.locals[s.Name]
		code = append(code, OpLocalSet, byte(idx))
		return code
	case *parser.ExprStmt:
		return c.compileExpr(s.Expr)
	}
	return nil
}

func (c *Compiler) compileExpr(e parser.Expr) []byte {
	switch e := e.(type) {
	case *parser.IntLit:
		return append([]byte{0x41}, sleb128(e.Value)...) // i32.const
	case *parser.StringLit:
		addr := c.strings.Add(e.Value)
		return append([]byte{0x41}, sleb128(int64(addr))...) // i32.const addr
	case *parser.Ident:
		idx := c.locals[e.Name]
		return []byte{OpLocalGet, byte(idx)}
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
		}
		return code
	case *parser.IfExpr:
		var code []byte
		code = append(code, c.compileExpr(e.Cond)...)
		code = append(code, OpIf, 0x7f) // if with i32 result
		code = append(code, c.compileBlock(e.Then)...)
		if e.Else != nil {
			code = append(code, OpElse)
			code = append(code, c.compileBlock(e.Else)...)
		}
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
		code = append(code, OpI32Eqz)     // invert condition
		code = append(code, OpBrIf, 1)    // br_if to block (exit)
		code = append(code, c.compileBlock(e.Body)...)
		code = append(code, OpBr, 0)      // br to loop (continue)
		code = append(code, OpEnd)        // end loop
		code = append(code, OpEnd)        // end block
		return code
	case *parser.Block:
		return c.compileBlock(e)
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
		default:
			idx := c.funcIdx[e.Name]
			code = append(code, OpCall, byte(idx))
		}
		return code
	}
	return nil
}
