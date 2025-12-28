package codegen

import "github.com/alexisbouchez/lingua/parser"

// WASM opcodes
const (
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
	OpIf       = 0x04
	OpElse     = 0x05
	OpEnd      = 0x0b
)

type Compiler struct {
	fn        *parser.FnDecl
	locals    map[string]int
	numLocals int
}

func Compile(fn *parser.FnDecl) (code []byte, numLocals int) {
	c := &Compiler{
		fn:     fn,
		locals: make(map[string]int),
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
	case *parser.ExprStmt:
		return c.compileExpr(s.Expr)
	}
	return nil
}

func (c *Compiler) compileExpr(e parser.Expr) []byte {
	switch e := e.(type) {
	case *parser.IntLit:
		return append([]byte{0x41}, sleb128(e.Value)...) // i32.const
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
	case *parser.Block:
		return c.compileBlock(e)
	}
	return nil
}
