package codegen

import "github.com/alexisbouchez/lingua/parser"

// WASM opcodes
const (
	OpLocalGet = 0x20
	OpI32Add   = 0x6a
	OpI32Sub   = 0x6b
	OpI32Mul   = 0x6c
	OpI32Div   = 0x6d
)

type Compiler struct {
	fn     *parser.FnDecl
	locals map[string]int
}

func Compile(fn *parser.FnDecl) []byte {
	c := &Compiler{
		fn:     fn,
		locals: make(map[string]int),
	}

	// Map params to local indices
	for i, p := range fn.Params {
		c.locals[p.Name] = i
	}

	return c.compileExpr(fn.Body)
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
		}
		return code
	}
	return nil
}
