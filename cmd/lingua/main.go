package main

import (
	"fmt"
	"os"
	"strings"

	"github.com/alexisbouchez/lingua/codegen"
	"github.com/alexisbouchez/lingua/component"
	"github.com/alexisbouchez/lingua/parser"
	"github.com/alexisbouchez/lingua/types"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: lingua <file.lingua> [output.wasm|output.wat|output.component.wasm]")
		os.Exit(1)
	}

	src, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	p := parser.New(string(src))
	f := p.ParseFile()

	if p.Errors().HasErrors() {
		fmt.Fprintln(os.Stderr, p.Errors().Format())
		os.Exit(1)
	}

	// Type check
	checker := types.NewChecker()
	typeErrors := checker.Check(f)
	if len(typeErrors) > 0 {
		for _, err := range typeErrors {
			fmt.Fprintln(os.Stderr, err)
		}
		os.Exit(1)
	}

	m := codegen.NewModule()
	m.AddMemory(1) // 1 page = 64KB, needed for WASI
	codegen.CompileFile(f, m)

	outFile := "out.wasm"
	if len(os.Args) > 2 {
		outFile = os.Args[2]
	}

	// Output format based on extension
	if strings.HasSuffix(outFile, ".wat") {
		// WAT text format
		if err := os.WriteFile(outFile, []byte(m.WAT()), 0644); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	} else if strings.HasSuffix(outFile, ".component.wasm") {
		// Component Model format (wraps core module)
		comp := component.New()
		comp.SetCoreModule(m.Bytes())
		if err := os.WriteFile(outFile, comp.Bytes(), 0644); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	} else {
		// Binary WASM (core module)
		if err := os.WriteFile(outFile, m.Bytes(), 0644); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	}

	fmt.Printf("compiled %s -> %s\n", os.Args[1], outFile)
}
