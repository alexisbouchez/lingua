package examples

import (
	"context"
	"os"
	"testing"

	"github.com/tetratelabs/wazero"
)

func TestRunAdd(t *testing.T) {
	wasm, err := os.ReadFile("add.wasm")
	if err != nil {
		t.Skip("add.wasm not found, run: lingua examples/add.lingua examples/add.wasm")
	}

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, wasm)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	add := mod.ExportedFunction("add")
	results, err := add.Call(ctx, 7, 8)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] != 15 {
		t.Fatalf("expected 15, got %d", results[0])
	}
}

func TestRunDoubleAdd(t *testing.T) {
	wasm, err := os.ReadFile("double_add.wasm")
	if err != nil {
		t.Skip("double_add.wasm not found")
	}

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, wasm)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	fn := mod.ExportedFunction("double_add")
	results, err := fn.Call(ctx, 3, 4)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	// (3 + 4) * 2 = 14
	if results[0] != 14 {
		t.Fatalf("expected 14, got %d", results[0])
	}
}

func TestRunMax(t *testing.T) {
	wasm, err := os.ReadFile("max.wasm")
	if err != nil {
		t.Skip("max.wasm not found")
	}

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, wasm)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	max := mod.ExportedFunction("max")

	results, err := max.Call(ctx, 10, 5)
	if err != nil {
		t.Fatalf("call: %v", err)
	}
	if results[0] != 10 {
		t.Fatalf("expected 10, got %d", results[0])
	}

	results, err = max.Call(ctx, 3, 7)
	if err != nil {
		t.Fatalf("call: %v", err)
	}
	if results[0] != 7 {
		t.Fatalf("expected 7, got %d", results[0])
	}
}

func TestRunSum(t *testing.T) {
	wasm, err := os.ReadFile("sum.wasm")
	if err != nil {
		t.Skip("sum.wasm not found")
	}

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, wasm)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	sum := mod.ExportedFunction("sum")

	// sum(5) = 0+1+2+3+4 = 10
	results, err := sum.Call(ctx, 5, 0)
	if err != nil {
		t.Fatalf("call: %v", err)
	}
	if results[0] != 10 {
		t.Fatalf("expected 10, got %d", results[0])
	}

	// sum(10) = 0+1+2+...+9 = 45
	results, err = sum.Call(ctx, 10, 0)
	if err != nil {
		t.Fatalf("call: %v", err)
	}
	if results[0] != 45 {
		t.Fatalf("expected 45, got %d", results[0])
	}
}
