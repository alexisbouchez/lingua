// Package repl provides a Read-Eval-Print Loop for the Lingua programming language
package repl

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/alexisbouchez/lingua/codegen"
	"github.com/alexisbouchez/lingua/parser"
	"github.com/alexisbouchez/lingua/types"
)

// REPL represents a Read-Eval-Print Loop instance
type REPL struct {
	reader      *bufio.Reader
	writer      io.Writer
	parser      *parser.Parser
	module      *codegen.Module
	variables   map[string]int32
	funcIdx     map[string]int
	globalIdx   map[string]int
	structs     map[string]*codegen.StructInfo
	strings     *codegen.StringTable
	checker     *types.Checker
}

// New creates a new REPL instance
func New(reader io.Reader, writer io.Writer) *REPL {
	return &REPL{
		reader:    bufio.NewReader(reader),
		writer:    writer,
		variables: make(map[string]int32),
		funcIdx:   make(map[string]int),
		globalIdx: make(map[string]int),
		structs:   make(map[string]*codegen.StructInfo),
		strings:   codegen.NewStringTable(0),
		checker:   types.NewChecker(),
	}
}

// Start begins the REPL loop
func (r *REPL) Start() {
	fmt.Fprintln(r.writer, "Lingua REPL - Interactive WebAssembly Language")
	fmt.Fprintln(r.writer, "Type :help for help, :quit to exit")

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

		// Trim whitespace and handle commands
		input = strings.TrimSpace(input)
		if input == "" {
			continue
		}

		// Handle REPL commands
		if strings.HasPrefix(input, ":") {
			r.handleCommand(input)
			continue
		}

		// Evaluate the input
		result, err := r.Evaluate(input)
		if err != nil {
			fmt.Fprintf(r.writer, "Error: %v\n", err)
		} else {
			fmt.Fprintf(r.writer, "=> %d\n", result)
		}
	}
}

// handleCommand processes REPL commands
func (r *REPL) handleCommand(cmd string) {
	switch cmd {
	case ":help":
		fmt.Fprintln(r.writer, "Available commands:")
		fmt.Fprintln(r.writer, "  :help      - Show this help message")
		fmt.Fprintln(r.writer, "  :quit      - Exit the REPL")
		fmt.Fprintln(r.writer, "  :vars      - Show defined variables")
		fmt.Fprintln(r.writer, "  :clear     - Clear all variables")
	case ":quit", ":exit":
		fmt.Fprintln(r.writer, "Goodbye!")
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
	case ":clear":
		r.variables = make(map[string]int32)
		fmt.Fprintln(r.writer, "Variables cleared")
	default:
		fmt.Fprintf(r.writer, "Unknown command: %s\n", cmd)
	}
}

// Evaluate parses and executes Lingua code
func (r *REPL) Evaluate(input string) (int32, error) {
	// Try to compile and execute the input as Lingua code
	if r.tryCompileAndExecute(input) {
		// If compilation was successful, return the result
		// For now, we'll use the simple evaluator as fallback
		// In a real implementation, we would execute the compiled WASM
	}
	
	// For now, handle simple expressions and variable assignments
	if strings.Contains(input, "=") {
		// Handle variable assignment: name = expression
		parts := strings.SplitN(input, "=", 2)
		if len(parts) != 2 {
			return 0, fmt.Errorf("invalid assignment: %s", input)
		}
		name := strings.TrimSpace(parts[0])
		expr := strings.TrimSpace(parts[1])
		
		// Evaluate the expression
		value, err := r.evaluateExpression(expr)
		if err != nil {
			return 0, err
		}
		
		// Store the variable
		r.variables[name] = value
		return value, nil
	}
	
	// Evaluate expression
	return r.evaluateExpression(input)
}

// tryCompileAndExecute attempts to compile and execute Lingua code
func (r *REPL) tryCompileAndExecute(input string) bool {
	// Create a function that wraps the input code
	fnCode := fmt.Sprintf("fn _repl_expr(): i32 { %s }", input)
	
	// Parse the function
	p := parser.New(fnCode)
	fn := p.ParseFn()
	if p.Errors().HasErrors() {
		// Parsing failed, but that's okay - we'll use the simple evaluator
		return false
	}
	
	// Type check the function
	file := &parser.File{Fns: []*parser.FnDecl{fn}}
	typeErrors := r.checker.Check(file)
	if len(typeErrors) > 0 {
		// Type checking failed
		return false
	}
	
	// Compilation successful!
	// In a real implementation, we would:
	// 1. Compile the function to WASM
	// 2. Execute it in a WASM runtime
	// 3. Return the result
	
	return true
}

// evaluateExpression evaluates a Lingua expression
func (r *REPL) evaluateExpression(expr string) (int32, error) {
	// Handle simple arithmetic expressions
	// This is a simplified evaluator - in a real implementation,
	// we would parse the expression properly and compile it
	
	// Check if it's a variable reference
	if val, ok := r.variables[expr]; ok {
		return val, nil
	}
	
	// Handle numeric literals
	var a, b int32
	
	// Simple parser for expressions like "a + b" or "42"
	if strings.Contains(expr, "+") {
		parts := strings.Fields(expr)
		if len(parts) != 3 {
			return 0, fmt.Errorf("invalid expression: %s", expr)
		}
		a = r.getValue(parts[0])
		b = r.getValue(parts[2])
		return a + b, nil
	} else if strings.Contains(expr, "-") {
		parts := strings.Fields(expr)
		if len(parts) != 3 {
			return 0, fmt.Errorf("invalid expression: %s", expr)
		}
		a = r.getValue(parts[0])
		b = r.getValue(parts[2])
		return a - b, nil
	} else if strings.Contains(expr, "*") {
		parts := strings.Fields(expr)
		if len(parts) != 3 {
			return 0, fmt.Errorf("invalid expression: %s", expr)
		}
		a = r.getValue(parts[0])
		b = r.getValue(parts[2])
		return a * b, nil
	} else if strings.Contains(expr, "/") {
		parts := strings.Fields(expr)
		if len(parts) != 3 {
			return 0, fmt.Errorf("invalid expression: %s", expr)
		}
		a = r.getValue(parts[0])
		b = r.getValue(parts[2])
		if b == 0 {
			return 0, fmt.Errorf("division by zero")
		}
		return a / b, nil
	}
	
	// Handle single numeric value
	return r.getValue(expr), nil
}

// getValue resolves a value (variable or literal)
func (r *REPL) getValue(s string) int32 {
	// Check if it's a variable
	if val, ok := r.variables[s]; ok {
		return val
	}
	
	// Parse as integer
	var result int32
	_, err := fmt.Sscanf(s, "%d", &result)
	if err != nil {
		return 0
	}
	return result
}