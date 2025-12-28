package codegen

import (
	"context"
	"testing"

	"github.com/alexisbouchez/lingua/parser"
	"github.com/tetratelabs/wazero"
)

func TestCompileAndRun(t *testing.T) {
	// Parse: fn add(a: i32, b: i32): i32 { a + b }
	p := parser.New("fn add(a: i32, b: i32): i32 { a + b }")
	fn := p.ParseFn()

	code := Compile(fn)

	m := NewModule()
	m.AddFunction("add", code)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	add := mod.ExportedFunction("add")
	results, err := add.Call(ctx, 10, 20)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] != 30 {
		t.Fatalf("expected 30, got %d", results[0])
	}
}

func TestCompileExpr(t *testing.T) {
	// fn calc(x: i32, y: i32): i32 { x * 2 + y }
	p := parser.New("fn calc(x: i32, y: i32): i32 { x * 2 + y }")
	fn := p.ParseFn()

	code := Compile(fn)

	m := NewModule()
	m.AddFunction("calc", code)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	calc := mod.ExportedFunction("calc")
	results, err := calc.Call(ctx, 5, 3)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	// 5 * 2 + 3 = 13
	if results[0] != 13 {
		t.Fatalf("expected 13, got %d", results[0])
	}
}
