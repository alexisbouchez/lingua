// REPL command-line interface for Lingua programming language
package main

import (
	"os"

	"github.com/alexisbouchez/lingua/repl"
)

func main() {
	// Create a new REPL instance
	repl := repl.NewREPL(os.Stdin, os.Stdout)

	// Start the REPL
	repl.Start()
}
