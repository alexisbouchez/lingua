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

	code, numLocals := Compile(fn)

	m := NewModule()
	m.AddFunction("add", code, numLocals)

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

	code, numLocals := Compile(fn)

	m := NewModule()
	m.AddFunction("calc", code, numLocals)

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

func TestCompileWithLocals(t *testing.T) {
	// fn foo(x: i32, y: i32): i32 { let z = x * 2; z + y }
	p := parser.New("fn foo(x: i32, y: i32): i32 { let z: i32 = x * 2; z + y }")
	fn := p.ParseFn()

	code, numLocals := Compile(fn)
	if numLocals != 1 {
		t.Fatalf("expected 1 local, got %d", numLocals)
	}

	m := NewModule()
	m.AddFunction("foo", code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	foo := mod.ExportedFunction("foo")
	results, err := foo.Call(ctx, 5, 3)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	// z = 5 * 2 = 10; 10 + 3 = 13
	if results[0] != 13 {
		t.Fatalf("expected 13, got %d", results[0])
	}
}

func TestCompileIfElse(t *testing.T) {
	// fn max(a: i32, b: i32): i32 { if a > b { a } else { b } }
	p := parser.New("fn max(a: i32, b: i32): i32 { if a > b { a } else { b } }")
	fn := p.ParseFn()

	code, numLocals := Compile(fn)

	m := NewModule()
	m.AddFunction("max", code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	max := mod.ExportedFunction("max")

	// max(10, 5) = 10
	results, err := max.Call(ctx, 10, 5)
	if err != nil {
		t.Fatalf("call: %v", err)
	}
	if results[0] != 10 {
		t.Fatalf("expected 10, got %d", results[0])
	}

	// max(3, 7) = 7
	results, err = max.Call(ctx, 3, 7)
	if err != nil {
		t.Fatalf("call: %v", err)
	}
	if results[0] != 7 {
		t.Fatalf("expected 7, got %d", results[0])
	}
}
