// Example demonstrating component model interface usage
package main

import (
	"fmt"
	"github.com/alexisbouchez/lingua/component"
)

func main() {
	// Create a new component
	comp := &component.Component{}

	// Define an interface
	httpHandler := component.InterfaceType{
		Name: "wasi:http/outgoing-handler",
		Version: "0.2.0",
		Functions: []component.FuncType{
			{
				Params: []component.NamedType{
					{Name: "request", Type: component.ValRecord},
				},
				Results: []component.NamedType{
					{Name: "response", Type: component.ValRecord},
				},
			},
		},
	}

	// Add the interface to the component
	comp.AddInterface(httpHandler)

	// Export the interface
	comp.AddInterfaceExport("wasi:http/outgoing-handler", httpHandler)

	// Import another interface
	logging := component.InterfaceType{
		Name: "wasi:logging/logging",
		Version: "0.2.0",
		Functions: []component.FuncType{
			{
				Params: []component.NamedType{
					{Name: "level", Type: component.ValU32},
					{Name: "message", Type: component.ValString},
				},
			},
		},
	}

	// Import the logging interface
	comp.AddInterfaceImport("wasi:logging", logging)

	fmt.Println("Component created with interfaces:")
	fmt.Printf("- Exported: %s\n", httpHandler.Name)
	fmt.Printf("- Imported: %s\n", logging.Name)
	fmt.Printf("Total types: %d\n", len(comp.Types()))
	fmt.Printf("Total imports: %d\n", len(comp.Imports()))
	fmt.Printf("Total exports: %d\n", len(comp.Exports()))
}