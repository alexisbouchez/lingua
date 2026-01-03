// Package repl provides a Read-Eval-Print Loop for the Lingua programming language
package repl

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/alexisbouchez/lingua/codegen"
	"github.com/alexisbouchez/lingua/parser"
	"github.com/alexisbouchez/lingua/types"
	"github.com/tetratelabs/wazero"
	"github.com/tetratelabs/wazero/imports/wasi_snapshot_preview1"
)

type REPL struct {
	reader    *bufio.Reader
	writer    io.Writer
	variables map[string]int32
	checker   *types.Checker
	runtime   wazero.Runtime
	ctx       context.Context
	funcs     map[string]*parser.FnDecl
}

func NewREPL(reader io.Reader, writer io.Writer) *REPL {
	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	return &REPL{
		reader:    bufio.NewReader(reader),
		writer:    writer,
		variables: make(map[string]int32),
		checker:   types.NewChecker(),
		runtime:   r,
		ctx:       ctx,
		funcs:     make(map[string]*parser.FnDecl),
	}
}

func (r *REPL) Close() {
	if r.runtime != nil {
		r.runtime.Close(r.ctx)
	}
}

func (r *REPL) Start() {
	fmt.Fprintln(r.writer, "Lingua REPL - Interactive WebAssembly Language")
	fmt.Fprintln(r.writer, "Type :help for help, :quit to exit")
	fmt.Fprintln(r.writer, "")

	for {
		fmt.Print(">>> ")
		input, err := r.reader.ReadString('\n')
		if err != nil {
			if err == io.EOF {
				fmt.Fprintln(r.writer, "\nGoodbye!")
				break
			}
			fmt.Fprintf(r.writer, "Error reading input: %v\n", err)
			continue
		}

		input = strings.TrimSpace(input)
		if input == "" {
			continue
		}

		if strings.HasPrefix(input, ":") {
			r.handleCommand(input)
			continue
		}

		result, err := r.Evaluate(input)
		if err != nil {
			fmt.Fprintf(r.writer, "Error: %v\n", err)
		} else {
			fmt.Fprintf(r.writer, "=> %d\n", result)
		}
	}
}

func (r *REPL) handleCommand(cmd string) {
	switch cmd {
	case ":help":
		fmt.Fprintln(r.writer, "Available commands:")
		fmt.Fprintln(r.writer, "  :help       - Show this help message")
		fmt.Fprintln(r.writer, "  :quit       - Exit the REPL")
		fmt.Fprintln(r.writer, "  :vars       - Show defined variables")
		fmt.Fprintln(r.writer, "  :funcs      - Show defined functions")
		fmt.Fprintln(r.writer, "  :clear      - Clear all variables")
		fmt.Fprintln(r.writer, "  :reset      - Reset REPL state (clears all)")
		fmt.Fprintln(r.writer, "  :load <file> - Load and run a .lingua file")
		fmt.Fprintln(r.writer, "  :types      - Show available builtin types")
		fmt.Fprintln(r.writer, "")
		fmt.Fprintln(r.writer, "Examples:")
		fmt.Fprintln(r.writer, "  1 + 2        => 3")
		fmt.Fprintln(r.writer, "  let x = 5    => defines variable x")
		fmt.Fprintln(r.writer, "  fn add(a, b)  => start function definition")
	case ":quit", ":exit":
		fmt.Fprintln(r.writer, "Goodbye!")
		r.Close()
		os.Exit(0)
	case ":vars":
		if len(r.variables) == 0 {
			fmt.Fprintln(r.writer, "No variables defined")
		} else {
			fmt.Fprintln(r.writer, "Defined variables:")
			for name, value := range r.variables {
				fmt.Fprintf(r.writer, "  %s = %d\n", name, value)
			}
		}
	case ":funcs":
		if len(r.funcs) == 0 {
			fmt.Fprintln(r.writer, "No functions defined")
		} else {
			fmt.Fprintln(r.writer, "Defined functions:")
			for name, fn := range r.funcs {
				params := make([]string, len(fn.Params))
				for i, p := range fn.Params {
					params[i] = fmt.Sprintf("%s: %s", p.Name, p.Type)
				}
				fmt.Fprintf(r.writer, "  fn %s(%s): %s\n", name, strings.Join(params, ", "), fn.Return)
			}
		}
	case ":clear":
		r.variables = make(map[string]int32)
		fmt.Fprintln(r.writer, "Variables cleared")
	case ":reset":
		r.variables = make(map[string]int32)
		r.funcs = make(map[string]*parser.FnDecl)
		r.checker = types.NewChecker()
		fmt.Fprintln(r.writer, "REPL state reset")
	case ":types":
		fmt.Fprintln(r.writer, "Available types:")
		fmt.Fprintln(r.writer, "  i32 - 32-bit signed integer")
		fmt.Fprintln(r.writer, "  i64 - 64-bit signed integer")
		fmt.Fprintln(r.writer, "  f32 - 32-bit floating point")
		fmt.Fprintln(r.writer, "  f64 - 64-bit floating point")
		fmt.Fprintln(r.writer, "")
		fmt.Fprintln(r.writer, "Available builtins:")
		fmt.Fprintln(r.writer, "  print(n)        - print integer")
		fmt.Fprintln(r.writer, "  print_str(addr, len) - print string")
		fmt.Fprintln(r.writer, "  read_char()     - read character from stdin")
		fmt.Fprintln(r.writer, "  read_line()     - read line from stdin")
		fmt.Fprintln(r.writer, "  random()        - generate random number")
		fmt.Fprintln(r.writer, "  time()          - get current time")
		fmt.Fprintln(r.writer, "  millis()        - get milliseconds since start")
		fmt.Fprintln(r.writer, "  malloc(size)    - allocate memory")
		fmt.Fprintln(r.writer, "  exit(code)      - exit with code")
	case cmd:
		if strings.HasPrefix(cmd, ":load ") {
			parts := strings.SplitN(cmd, " ", 2)
			if len(parts) != 2 {
				fmt.Fprintln(r.writer, "Usage: :load <filename>")
				return
			}
			r.loadFile(parts[1])
		} else {
			fmt.Fprintf(r.writer, "Unknown command: %s\n", cmd)
		}
	}
}

func (r *REPL) loadFile(filename string) {
	data, err := os.ReadFile(filename)
	if err != nil {
		fmt.Fprintf(r.writer, "Error reading file: %v\n", err)
		return
	}

	src := string(data)
	p := parser.New(src)
	file := p.ParseFile()
	if p.Errors().HasErrors() {
		fmt.Fprintf(r.writer, "Parse errors:\n")
		for _, e := range p.Errors().Errors {
			fmt.Fprintf(r.writer, "  %s\n", e)
		}
		return
	}

	m := codegen.NewModule()
	m.AddMemory(1)
	codegen.CompileFile(file, m, filename, false, false)

	mod, err := r.runtime.Instantiate(r.ctx, m.Bytes())
	if err != nil {
		fmt.Fprintf(r.writer, "Error instantiating module: %v\n", err)
		return
	}
	defer mod.Close(r.ctx)

	start := mod.ExportedFunction("_start")
	if start != nil {
		_, err := start.Call(r.ctx)
		if err != nil {
			fmt.Fprintf(r.writer, "Error executing _start: %v\n", err)
		}
	} else {
		fmt.Fprintln(r.writer, "File loaded (no _start function found)")
	}
}

func (r *REPL) Evaluate(input string) (int32, error) {
	input = strings.TrimSpace(input)

	if strings.HasPrefix(input, "fn ") {
		return r.defineFunction(input)
	}

	if strings.Contains(input, "=") && !strings.Contains(input, "==") {
		return r.handleAssignment(input)
	}

	if strings.HasSuffix(input, ")") {
		return r.callFunction(input)
	}

	return r.evaluateExpression(input)
}

func (r *REPL) defineFunction(input string) (int32, error) {
	lines := []string{input}

	for {
		fmt.Print("... ")
		line, err := r.reader.ReadString('\n')
		if err != nil {
			if err == io.EOF {
				break
			}
			return 0, err
		}
		line = strings.TrimSpace(line)
		if line = strings.TrimSpace(line); line == "" {
			continue
		}
		lines = append(lines, line)

		if strings.HasSuffix(line, "}") || strings.HasSuffix(line, "{") {
			break
		}
	}

	src := strings.Join(lines, "\n")
	p := parser.New(src)
	file := p.ParseFile()
	if p.Errors().HasErrors() {
		fmt.Fprintf(r.writer, "Parse errors:\n")
		for _, e := range p.Errors().Errors {
			fmt.Fprintf(r.writer, "  %s\n", e)
		}
		return 0, nil
	}

	typeErrors := r.checker.Check(file)
	if len(typeErrors) > 0 {
		for _, e := range typeErrors {
			fmt.Fprintf(r.writer, "Type error: %s\n", e.Message)
		}
		return 0, nil
	}

	for _, fn := range file.Fns {
		r.funcs[fn.Name] = fn
		fmt.Fprintf(r.writer, "Defined function: %s\n", fn.Name)
	}

	if len(file.Fns) > 0 {
		fn := file.Fns[0]
		if fn.Name != "_start" && fn.Return != "void" {
			return r.callFunction(fn.Name + "()")
		}
	}

	return 0, nil
}

func (r *REPL) handleAssignment(input string) (int32, error) {
	parts := strings.SplitN(input, "=", 2)
	if len(parts) != 2 {
		return 0, fmt.Errorf("invalid assignment: %s", input)
	}

	name := strings.TrimSpace(parts[0])
	if strings.HasPrefix(name, "let ") {
		name = strings.TrimSpace(name[4:])
	}
	expr := strings.TrimSpace(parts[1])

	value, err := r.evaluateExpression(expr)
	if err != nil {
		return 0, err
	}

	r.variables[name] = value
	return value, nil
}

func (r *REPL) evaluateExpression(expr string) (int32, error) {
	expr = strings.TrimSpace(expr)

	if val, ok := r.variables[expr]; ok {
		return val, nil
	}

	if strings.HasPrefix(expr, "0x") {
		var result int32
		_, err := fmt.Sscanf(expr, "0x%x", &result)
		if err == nil {
			return result, nil
		}
	}

	ops := []struct {
		sym string
		op  func(a, b int32) int32
	}{
		{"+", func(a, b int32) int32 { return a + b }},
		{"-", func(a, b int32) int32 { return a - b }},
		{"*", func(a, b int32) int32 { return a * b }},
		{"/", func(a, b int32) int32 { return a / b }},
		{"%", func(a, b int32) int32 { return a % b }},
	}

	for _, o := range ops {
		if idx := strings.Index(expr, o.sym); idx != -1 {
			a := strings.TrimSpace(expr[:idx])
			b := strings.TrimSpace(expr[idx+len(o.sym):])
			valA := r.getValue(a)
			valB := r.getValue(b)
			return o.op(valA, valB), nil
		}
	}

	return r.getValue(expr), nil
}

func (r *REPL) callFunction(call string) (int32, error) {
	openIdx := strings.Index(call, "(")
	if openIdx == -1 {
		return 0, fmt.Errorf("invalid function call: %s", call)
	}

	fnName := strings.TrimSpace(call[:openIdx])
	fn, ok := r.funcs[fnName]
	if !ok {
		return 0, fmt.Errorf("unknown function: %s", fnName)
	}

	argsStr := call[openIdx+1 : len(call)-1]
	var args []int32
	for _, arg := range strings.Split(argsStr, ",") {
		arg = strings.TrimSpace(arg)
		val := r.getValue(arg)
		args = append(args, val)
	}

	if len(args) != len(fn.Params) {
		return 0, fmt.Errorf("function %s expects %d arguments, got %d", fnName, len(fn.Params), len(args))
	}

	var fnDecl parser.FnDecl
	fnDecl.Name = "_repl_eval"
	fnDecl.Return = fn.Return
	fnDecl.Body = &parser.Block{
		Stmts: []parser.Stmt{
			&parser.ExprStmt{
				Expr: &parser.ReturnExpr{
					Value: &parser.CallExpr{
						Name: fnName,
						Args: make([]parser.Expr, len(args)),
					},
				},
			},
		},
	}
	for i, arg := range args {
		fnDecl.Body.Stmts[0].(*parser.ExprStmt).Expr.(*parser.ReturnExpr).Value.(*parser.CallExpr).Args[i] = &parser.IntLit{Value: int64(arg)}
	}

	file := &parser.File{Fns: []*parser.FnDecl{&fnDecl}}
	for _, f := range r.funcs {
		file.Fns = append(file.Fns, f)
	}

	m := codegen.NewModule()
	m.AddMemory(1)
	codegen.CompileFile(file, m, "repl", false, false)

	mod, err := r.runtime.Instantiate(r.ctx, m.Bytes())
	if err != nil {
		return 0, fmt.Errorf("compile error: %v", err)
	}
	defer mod.Close(r.ctx)

	fnExported := mod.ExportedFunction("_repl_eval")
	if fnExported == nil {
		return 0, fmt.Errorf("function not found")
	}

	results, err := fnExported.Call(r.ctx)
	if err != nil {
		return 0, err
	}

	if len(results) > 0 {
		return int32(results[0]), nil
	}
	return 0, nil
}

func (r *REPL) getValue(s string) int32 {
	s = strings.TrimSpace(s)

	if val, ok := r.variables[s]; ok {
		return val
	}

	if strings.HasPrefix(s, "0x") {
		var result int32
		_, err := fmt.Sscanf(s, "0x%x", &result)
		if err == nil {
			return result
		}
	}

	var result int32
	_, err := fmt.Sscanf(s, "%d", &result)
	if err != nil {
		return 0
	}
	return result
}
