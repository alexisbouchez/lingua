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
	structs   map[string]*StructType
	globals   map[string]Type
	locals    map[string]Type
	localTyps map[string]string // maps local variable name to struct type name
	returnTyp Type
}

// FuncType represents a function's type signature
type FuncType struct {
	Params []Type
	Return Type
}

// StructType represents a struct's type information
type StructType struct {
	Name   string
	Fields map[string]Type
	Order  []string // field order for memory layout
}

// NewChecker creates a new type checker
func NewChecker() *Checker {
	return &Checker{
		funcs:     make(map[string]*FuncType),
		structs:   make(map[string]*StructType),
		globals:   make(map[string]Type),
		locals:    make(map[string]Type),
		localTyps: make(map[string]string),
	}
}

// Check performs type checking on a file
func (c *Checker) Check(file *parser.File) []TypeError {
	c.errors = nil

	// Collect struct definitions
	for _, s := range file.Structs {
		fields := make(map[string]Type)
		var order []string
		for _, f := range s.Fields {
			typ := c.parseTypeWithStructs(f.Type)
			if typ == Unknown {
				c.addError(fmt.Sprintf("unknown type '%s' for field '%s' in struct '%s'", f.Type, f.Name, s.Name), 0)
			}
			fields[f.Name] = typ
			order = append(order, f.Name)
		}
		c.structs[s.Name] = &StructType{Name: s.Name, Fields: fields, Order: order}
	}

	// First pass: collect function signatures
	for _, fn := range file.Fns {
		params := make([]Type, len(fn.Params))
		for i, p := range fn.Params {
			params[i] = c.parseTypeWithStructs(p.Type)
			if params[i] == Unknown {
				c.addError(fmt.Sprintf("unknown parameter type '%s' for parameter '%s'", p.Type, p.Name), 0)
			}
		}
		retType := c.parseTypeWithStructs(fn.Return)
		if retType == Unknown && fn.Return != "" {
			c.addError(fmt.Sprintf("unknown return type '%s' for function '%s'", fn.Return, fn.Name), 0)
		}
		c.funcs[fn.Name] = &FuncType{Params: params, Return: retType}
	}

	// Collect global types
	for _, g := range file.Globals {
		typ := c.parseTypeWithStructs(g.Type)
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

// parseTypeWithStructs parses a type string, recognizing struct types
func (c *Checker) parseTypeWithStructs(s string) Type {
	// Check if it's a struct type
	if _, ok := c.structs[s]; ok {
		return I32 // Structs are represented as i32 pointers
	}
	return ParseType(s)
}

func (c *Checker) checkFunc(fn *parser.FnDecl) {
	// Set up local scope
	c.locals = make(map[string]Type)
	c.localTyps = make(map[string]string)
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
		declType := c.parseTypeWithStructs(s.Type)
		if declType == Unknown && s.Type != "" {
			c.addError(fmt.Sprintf("unknown type '%s' for variable '%s'", s.Type, s.Name), 0)
			return
		}

		// Track struct type name for field access
		if _, ok := c.structs[s.Type]; ok {
			c.localTyps[s.Name] = s.Type
		}

		if s.Value != nil {
			valueType := c.checkExpr(s.Value)
			// For struct literals, check if the type matches
			if lit, ok := s.Value.(*parser.StructLit); ok {
				if s.Type != "" && s.Type != lit.Name {
					c.addError(fmt.Sprintf("cannot assign %s to variable '%s' of type %s",
						lit.Name, s.Name, s.Type), 0)
				}
				c.localTyps[s.Name] = lit.Name
			} else if declType != Unknown && valueType != declType {
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

	case *parser.FieldAssignStmt:
		c.checkExpr(s.Expr)
		c.checkExpr(s.Value)
		// TODO: check field exists and type matches

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

	case *parser.StructLit:
		st, ok := c.structs[e.Name]
		if !ok {
			c.addError(fmt.Sprintf("undefined struct type '%s'", e.Name), 0)
			return Unknown
		}
		// Check all fields are valid
		for _, f := range e.Fields {
			if _, ok := st.Fields[f.Name]; !ok {
				c.addError(fmt.Sprintf("unknown field '%s' in struct '%s'", f.Name, e.Name), 0)
			}
			c.checkExpr(f.Value)
		}
		return I32 // structs are represented as i32 pointers

	case *parser.FieldExpr:
		c.checkExpr(e.Expr)
		// Try to find the struct type
		if ident, ok := e.Expr.(*parser.Ident); ok {
			if structName, ok := c.localTyps[ident.Name]; ok {
				if st, ok := c.structs[structName]; ok {
					if fieldType, ok := st.Fields[e.Field]; ok {
						return fieldType
					}
					c.addError(fmt.Sprintf("unknown field '%s' in struct '%s'", e.Field, structName), 0)
				}
			}
		}
		return I32 // assume i32 for field access
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
		"http_get", "http_post", "http_request",
		"argc", "envc", "stdin_read", "stdout_write", "stderr_write":
		return I32

	// String builtins
	case "str_eq", "str_copy":
		return I32

	// Error handling builtins
	case "panic":
		return Void // doesn't return
	case "assert":
		return Void // returns nothing on success, exits on failure

	// Collection builtins - List
	case "list_new":
		return I32 // returns pointer to list
	case "list_push":
		return I32 // void-like, returns 0
	case "list_pop":
		return I32 // returns popped element
	case "list_get":
		return I32 // returns element at index
	case "list_set":
		return I32 // void-like, returns 0
	case "list_len":
		return I32 // returns length

	// Collection builtins - Map
	case "map_new":
		return I32 // returns pointer to map
	case "map_set":
		return I32 // void-like, returns 0
	case "map_get":
		return I32 // returns value for key
	case "map_has":
		return I32 // returns 1 if key exists, 0 otherwise

	// Collection builtins - Set
	case "set_new":
		return I32 // returns pointer to set
	case "set_add":
		return I32 // void-like, returns 0
	case "set_has":
		return I32 // returns 1 if value exists, 0 otherwise
	case "set_remove":
		return I32 // void-like, returns 0
	case "set_len":
		return I32 // returns number of elements

	case "drop":
		return Void
	}

	return Unknown
}

func (c *Checker) addError(msg string, line int) {
	c.errors = append(c.errors, TypeError{Message: msg, Line: line})
}
