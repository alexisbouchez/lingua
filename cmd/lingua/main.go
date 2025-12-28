package main

import (
	"fmt"
	"os"

	"github.com/alexisbouchez/lingua/codegen"
	"github.com/alexisbouchez/lingua/parser"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: lingua <file.lingua>")
		os.Exit(1)
	}

	src, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	p := parser.New(string(src))
	fn := p.ParseFn()

	code, numLocals := codegen.Compile(fn)

	m := codegen.NewModule()
	m.AddFunction(fn.Name, code, numLocals)

	outFile := "out.wasm"
	if len(os.Args) > 2 {
		outFile = os.Args[2]
	}

	if err := os.WriteFile(outFile, m.Bytes(), 0644); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	fmt.Printf("compiled %s -> %s\n", os.Args[1], outFile)
}
