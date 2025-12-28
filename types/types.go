package types

import (
	"fmt"

	"github.com/alexisbouchez/lingua/parser"
)

// Type represents a Lingua type
type Type int

const (
	Unknown Type = iota
	Void
	I32
	I64
	F32
	F64
	Bool   // Internal type for conditions
	String // Pointer + length pair
)

func (t Type) String() string {
	switch t {
	case Unknown:
		return "unknown"
	case Void:
		return "void"
	case I32:
		return "i32"
	case I64:
		return "i64"
	case F32:
		return "f32"
	case F64:
		return "f64"
	case Bool:
		return "bool"
	case String:
		return "string"
	default:
		return "invalid"
	}
}

// ParseType converts a type string to a Type
func ParseType(s string) Type {
	switch s {
	case "i32":
		return I32
	case "i64":
		return I64
	case "f32":
		return F32
	case "f64":
		return F64
	case "void", "":
		return Void
	default:
		return Unknown
	}
}

// TypeError represents a type checking error
type TypeError struct {
	Message string
	Line    int
	Column  int
}

func (e TypeError) Error() string {
	if e.Line > 0 {
		return fmt.Sprintf("type error at line %d: %s", e.Line, e.Message)
	}
	return fmt.Sprintf("type error: %s", e.Message)
}

// Checker performs type checking on an AST
type Checker struct {
	errors    []TypeError
	funcs     map[string]*FuncType
	globals   map[string]Type
	locals    map[string]Type
	returnTyp Type
}

// FuncType represents a function's type signature
type FuncType struct {
	Params []Type
	Return Type
}

// NewChecker creates a new type checker
func NewChecker() *Checker {
	return &Checker{
		funcs:   make(map[string]*FuncType),
		globals: make(map[string]Type),
		locals:  make(map[string]Type),
	}
}

// Check performs type checking on a file
func (c *Checker) Check(file *parser.File) []TypeError {
	c.errors = nil

	// First pass: collect function signatures
	for _, fn := range file.Fns {
		params := make([]Type, len(fn.Params))
		for i, p := range fn.Params {
			params[i] = ParseType(p.Type)
			if params[i] == Unknown {
				c.addError(fmt.Sprintf("unknown parameter type '%s' for parameter '%s'", p.Type, p.Name), 0)
			}
		}
		retType := ParseType(fn.Return)
		if retType == Unknown && fn.Return != "" {
			c.addError(fmt.Sprintf("unknown return type '%s' for function '%s'", fn.Return, fn.Name), 0)
		}
		c.funcs[fn.Name] = &FuncType{Params: params, Return: retType}
	}

	// Collect global types
	for _, g := range file.Globals {
		typ := ParseType(g.Type)
		if typ == Unknown {
			c.addError(fmt.Sprintf("unknown type '%s' for global '%s'", g.Type, g.Name), 0)
		}
		c.globals[g.Name] = typ
	}

	// Second pass: check function bodies
	for _, fn := range file.Fns {
		c.checkFunc(fn)
	}

	return c.errors
}

func (c *Checker) checkFunc(fn *parser.FnDecl) {
	// Set up local scope
	c.locals = make(map[string]Type)
	c.returnTyp = ParseType(fn.Return)

	// Add parameters to locals
	for _, p := range fn.Params {
		c.locals[p.Name] = ParseType(p.Type)
	}

	// Check body
	if fn.Body != nil {
		bodyType := c.checkBlock(fn.Body)

		// Check return type matches
		if c.returnTyp != Void && bodyType != c.returnTyp {
			c.addError(fmt.Sprintf("function '%s' should return %s but body returns %s",
				fn.Name, c.returnTyp, bodyType), 0)
		}
	}
}

func (c *Checker) checkBlock(block *parser.Block) Type {
	for _, stmt := range block.Stmts {
		c.checkStmt(stmt)
	}

	if block.Expr != nil {
		return c.checkExpr(block.Expr)
	}
	return Void
}

func (c *Checker) checkStmt(stmt parser.Stmt) {
	switch s := stmt.(type) {
	case *parser.LetStmt:
		declType := ParseType(s.Type)
		if declType == Unknown && s.Type != "" {
			c.addError(fmt.Sprintf("unknown type '%s' for variable '%s'", s.Type, s.Name), 0)
			return
		}

		if s.Value != nil {
			valueType := c.checkExpr(s.Value)
			if declType != Unknown && valueType != declType {
				c.addError(fmt.Sprintf("cannot assign %s to variable '%s' of type %s",
					valueType, s.Name, declType), 0)
			}
			if declType == Unknown {
				declType = valueType
			}
		}
		c.locals[s.Name] = declType

	case *parser.AssignStmt:
		valueType := c.checkExpr(s.Value)
		varType, ok := c.locals[s.Name]
		if !ok {
			varType, ok = c.globals[s.Name]
		}
		if !ok {
			c.addError(fmt.Sprintf("undefined variable '%s'", s.Name), 0)
			return
		}
		if valueType != varType {
			c.addError(fmt.Sprintf("cannot assign %s to variable '%s' of type %s",
				valueType, s.Name, varType), 0)
		}

	case *parser.IndexAssignStmt:
		c.checkExpr(s.Array)
		indexType := c.checkExpr(s.Index)
		if indexType != I32 {
			c.addError(fmt.Sprintf("array index must be i32, got %s", indexType), 0)
		}
		c.checkExpr(s.Value)

	case *parser.ExprStmt:
		c.checkExpr(s.Expr)
	}
}

func (c *Checker) checkExpr(expr parser.Expr) Type {
	switch e := expr.(type) {
	case *parser.IntLit:
		return I32

	case *parser.StringLit:
		return String

	case *parser.Ident:
		if typ, ok := c.locals[e.Name]; ok {
			return typ
		}
		if typ, ok := c.globals[e.Name]; ok {
			return typ
		}
		c.addError(fmt.Sprintf("undefined variable '%s'", e.Name), 0)
		return Unknown

	case *parser.BinaryExpr:
		left := c.checkExpr(e.Left)
		right := c.checkExpr(e.Right)

		switch e.Op {
		case "+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>":
			if left != right {
				c.addError(fmt.Sprintf("type mismatch in binary expression: %s %s %s",
					left, e.Op, right), 0)
			}
			return left

		case "==", "!=", "<", ">", "<=", ">=":
			if left != right {
				c.addError(fmt.Sprintf("type mismatch in comparison: %s %s %s",
					left, e.Op, right), 0)
			}
			return I32 // comparisons return i32 (0 or 1)

		case "&&", "||":
			return I32 // logical ops return i32
		}
		return Unknown

	case *parser.UnaryExpr:
		operand := c.checkExpr(e.Expr)
		switch e.Op {
		case "-":
			return operand
		case "!":
			return I32
		}
		return Unknown

	case *parser.CallExpr:
		return c.checkCall(e)

	case *parser.IfExpr:
		condType := c.checkExpr(e.Cond)
		if condType != I32 && condType != Bool {
			c.addError(fmt.Sprintf("condition must be i32, got %s", condType), 0)
		}

		thenType := Void
		if e.Then != nil {
			thenType = c.checkBlock(e.Then)
		}

		elseType := Void
		if e.Else != nil {
			elseType = c.checkBlock(e.Else)
		}

		if e.Else != nil && thenType != elseType {
			c.addError(fmt.Sprintf("if branches have different types: %s vs %s",
				thenType, elseType), 0)
		}

		return thenType

	case *parser.LoopExpr:
		if e.Cond != nil {
			condType := c.checkExpr(e.Cond)
			if condType != I32 && condType != Bool {
				c.addError(fmt.Sprintf("loop condition must be i32, got %s", condType), 0)
			}
		}
		if e.Body != nil {
			c.checkBlock(e.Body)
		}
		return Void

	case *parser.Block:
		return c.checkBlock(e)

	case *parser.BreakExpr, *parser.ContinueExpr:
		return Void

	case *parser.ReturnExpr:
		if e.Value != nil {
			retType := c.checkExpr(e.Value)
			if retType != c.returnTyp {
				c.addError(fmt.Sprintf("return type mismatch: expected %s, got %s",
					c.returnTyp, retType), 0)
			}
			return retType
		}
		return Void

	case *parser.ArrayLit:
		if len(e.Elements) == 0 {
			return I32 // default element type
		}
		elemType := c.checkExpr(e.Elements[0])
		for i := 1; i < len(e.Elements); i++ {
			t := c.checkExpr(e.Elements[i])
			if t != elemType {
				c.addError(fmt.Sprintf("array element type mismatch: expected %s, got %s",
					elemType, t), 0)
			}
		}
		return I32 // arrays are represented as i32 pointers

	case *parser.IndexExpr:
		c.checkExpr(e.Array)
		indexType := c.checkExpr(e.Index)
		if indexType != I32 {
			c.addError(fmt.Sprintf("array index must be i32, got %s", indexType), 0)
		}
		return I32 // element type (assuming i32 array)
	}

	return Unknown
}

func (c *Checker) checkCall(call *parser.CallExpr) Type {
	// Check for builtin functions
	builtinType := c.builtinType(call.Name, call.Args)
	if builtinType != Unknown {
		return builtinType
	}

	// Check user-defined functions
	fn, ok := c.funcs[call.Name]
	if !ok {
		c.addError(fmt.Sprintf("undefined function '%s'", call.Name), 0)
		return Unknown
	}

	if len(call.Args) != len(fn.Params) {
		c.addError(fmt.Sprintf("function '%s' expects %d arguments, got %d",
			call.Name, len(fn.Params), len(call.Args)), 0)
		return fn.Return
	}

	for i, arg := range call.Args {
		argType := c.checkExpr(arg)
		if argType != fn.Params[i] {
			c.addError(fmt.Sprintf("argument %d of '%s' expects %s, got %s",
				i+1, call.Name, fn.Params[i], argType), 0)
		}
	}

	return fn.Return
}

func (c *Checker) builtinType(name string, args []parser.Expr) Type {
	switch name {
	// Math builtins returning i32
	case "abs", "min", "max", "mod", "pow", "sqrt", "random",
		"sign", "negate", "clamp", "is_even", "is_odd", "square", "cube", "log2",
		"and", "or", "xor", "shl", "shr", "clz", "ctz", "popcnt", "rotl", "rotr":
		return I32

	// Time builtins returning i32
	case "time", "millis":
		return I32

	// I/O builtins
	case "print", "print_str", "println", "print_int", "write_char":
		return I32

	case "read_char":
		return I32

	// Memory builtins
	case "load", "malloc":
		return I32

	case "store", "memcpy":
		return I32

	// WASI file operations
	case "open", "close", "read", "write", "seek", "sync", "stat",
		"mkdir", "rmdir", "unlink", "rename", "exit", "yield", "raise",
		"fdstat", "fstat", "readdir", "datasync", "allocate", "advise", "link",
		"tell", "symlink", "readlink", "prestat_get", "prestat_dir_name",
		"set_fd_flags", "get_args_sizes", "get_args", "get_environ_sizes", "get_environ",
		"sock_recv", "sock_send", "sock_shutdown",
		"http_get", "http_post", "http_request":
		return I32

	// String builtins
	case "str_eq", "str_copy":
		return I32

	case "drop":
		return Void
	}

	return Unknown
}

func (c *Checker) addError(msg string, line int) {
	c.errors = append(c.errors, TypeError{Message: msg, Line: line})
}
