package examples

import (
	"bytes"
	"context"
	"os"
	"testing"

	"github.com/tetratelabs/wazero"
	"github.com/tetratelabs/wazero/imports/wasi_snapshot_preview1"
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

func TestRunCall(t *testing.T) {
	wasm, err := os.ReadFile("call.wasm")
	if err != nil {
		t.Skip("call.wasm not found")
	}

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, wasm)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	fn := mod.ExportedFunction("sum_of_squares")
	// 3^2 + 4^2 = 9 + 16 = 25
	results, err := fn.Call(ctx, 3, 4)
	if err != nil {
		t.Fatalf("call: %v", err)
	}
	if results[0] != 25 {
		t.Fatalf("expected 25, got %d", results[0])
	}
}

func TestRunHello(t *testing.T) {
	wasm, err := os.ReadFile("hello.wasm")
	if err != nil {
		t.Skip("hello.wasm not found")
	}

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	var stdout bytes.Buffer
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	config := wazero.NewModuleConfig().WithStdout(&stdout)
	_, err = r.InstantiateWithConfig(ctx, wasm, config)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	if stdout.String() != "Hello, World!\n" {
		t.Fatalf("expected 'Hello, World!\\n', got %q", stdout.String())
	}
}

func TestRunCountdown(t *testing.T) {
	wasm, err := os.ReadFile("countdown.wasm")
	if err != nil {
		t.Skip("countdown.wasm not found")
	}

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	var stdout bytes.Buffer
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	config := wazero.NewModuleConfig().WithStdout(&stdout)
	_, err = r.InstantiateWithConfig(ctx, wasm, config)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	expected := "10\n9\n8\n7\n6\n5\n4\n3\n2\n1\nLiftoff!\n"
	if stdout.String() != expected {
		t.Fatalf("expected %q, got %q", expected, stdout.String())
	}
}

func TestRunHello2(t *testing.T) {
	wasm, err := os.ReadFile("hello2.wasm")
	if err != nil {
		t.Skip("hello2.wasm not found")
	}

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	var stdout bytes.Buffer
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	config := wazero.NewModuleConfig().WithStdout(&stdout)
	_, err = r.InstantiateWithConfig(ctx, wasm, config)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	if stdout.String() != "Hello, World!\n" {
		t.Fatalf("expected 'Hello, World!\\n', got %q", stdout.String())
	}
}

func TestRunFizzBuzz(t *testing.T) {
	wasm, err := os.ReadFile("fizzbuzz.wasm")
	if err != nil {
		t.Skip("fizzbuzz.wasm not found")
	}

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	var stdout bytes.Buffer
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	config := wazero.NewModuleConfig().WithStdout(&stdout)
	_, err = r.InstantiateWithConfig(ctx, wasm, config)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	expected := `1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
16
17
Fizz
19
Buzz
`
	if stdout.String() != expected {
		t.Fatalf("expected:\n%s\ngot:\n%s", expected, stdout.String())
	}
}
