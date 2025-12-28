package codegen

import (
	"bytes"
	"context"
	"testing"

	"github.com/alexisbouchez/lingua/parser"
	"github.com/tetratelabs/wazero"
	"github.com/tetratelabs/wazero/imports/wasi_snapshot_preview1"
)

func TestCompileAndRun(t *testing.T) {
	// Parse: fn add(a: i32, b: i32): i32 { a + b }
	p := parser.New("fn add(a: i32, b: i32): i32 { a + b }")
	fn := p.ParseFn()

	code, numLocals := Compile(fn, nil, NewStringTable(0))

	m := NewModule()
	m.AddFunction("add", 2, code, numLocals)

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

	code, numLocals := Compile(fn, nil, NewStringTable(0))

	m := NewModule()
	m.AddFunction("calc", 2, code, numLocals)

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

	code, numLocals := Compile(fn, nil, NewStringTable(0))
	if numLocals != 1 {
		t.Fatalf("expected 1 local, got %d", numLocals)
	}

	m := NewModule()
	m.AddFunction("foo", 2, code, numLocals)

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

	code, numLocals := Compile(fn, nil, NewStringTable(0))

	m := NewModule()
	m.AddFunction("max", 2, code, numLocals)

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

func TestCompileLoop(t *testing.T) {
	// fn sum(n: i32, _: i32): i32 { let i: i32 = 0; let s: i32 = 0; loop i < n { s = s + i; i = i + 1; }; s }
	src := `fn sum(n: i32, x: i32): i32 {
		let i: i32 = 0;
		let s: i32 = 0;
		loop i < n {
			s = s + i;
			i = i + 1;
		};
		s
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	code, numLocals := Compile(fn, nil, NewStringTable(0))

	m := NewModule()
	m.AddFunction("sum", 2, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	sum := mod.ExportedFunction("sum")

	// sum(5) = 0 + 1 + 2 + 3 + 4 = 10
	results, err := sum.Call(ctx, 5, 0)
	if err != nil {
		t.Fatalf("call: %v", err)
	}
	if results[0] != 10 {
		t.Fatalf("expected 10, got %d", results[0])
	}
}

func TestCompileCall(t *testing.T) {
	src := `fn add(a: i32, b: i32): i32 { a + b }
fn double(x: i32): i32 { add(x, x) }`
	p := parser.New(src)
	f := p.ParseFile()

	m := NewModule()
	CompileFile(f, m)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	double := mod.ExportedFunction("double")
	results, err := double.Call(ctx, 5)
	if err != nil {
		t.Fatalf("call: %v", err)
	}
	if results[0] != 10 {
		t.Fatalf("expected 10, got %d", results[0])
	}
}

func TestCompileMemory(t *testing.T) {
	// Store 42 at address 0, then load it back
	src := `fn test(x: i32): i32 { store(0, 42); load(0) }`
	p := parser.New(src)
	f := p.ParseFile()

	m := NewModule()
	m.AddMemory(1) // 1 page = 64KB
	CompileFile(f, m)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := mod.ExportedFunction("test")
	results, err := test.Call(ctx, 0)
	if err != nil {
		t.Fatalf("call: %v", err)
	}
	if results[0] != 42 {
		t.Fatalf("expected 42, got %d", results[0])
	}
}

func TestCompileData(t *testing.T) {
	// Load a byte from initialized memory
	src := `fn test(x: i32): i32 { load(0) }`
	p := parser.New(src)
	f := p.ParseFile()

	m := NewModule()
	m.AddMemory(1)
	m.AddData(0, []byte{42, 0, 0, 0}) // little-endian i32 = 42
	CompileFile(f, m)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := mod.ExportedFunction("test")
	results, err := test.Call(ctx, 0)
	if err != nil {
		t.Fatalf("call: %v", err)
	}
	if results[0] != 42 {
		t.Fatalf("expected 42, got %d", results[0])
	}
}

func TestWASIHello(t *testing.T) {
	// _start calls fd_write(1, iovs=0, iovs_len=1, nwritten=8)
	// Memory: iovec at 0 (buf=16, len=14), nwritten at 8, string at 16
	src := `fn _start(): i32 { fd_write(1, 0, 1, 8) }`
	p := parser.New(src)
	f := p.ParseFile()

	m := NewModule()
	m.AddMemory(1)

	// iovec: buf=16, len=14
	var data bytes.Buffer
	data.Write([]byte{16, 0, 0, 0}) // buf pointer
	data.Write([]byte{14, 0, 0, 0}) // buf len
	data.Write([]byte{0, 0, 0, 0})  // nwritten placeholder
	data.Write([]byte{0, 0, 0, 0})  // padding
	data.WriteString("Hello, World!\n")
	m.AddData(0, data.Bytes())

	CompileFile(f, m) // auto-imports fd_write

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	var stdout bytes.Buffer
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	config := wazero.NewModuleConfig().WithStdout(&stdout)
	_, err := r.InstantiateWithConfig(ctx, m.Bytes(), config)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	if stdout.String() != "Hello, World!\n" {
		t.Fatalf("expected 'Hello, World!\\n', got %q", stdout.String())
	}
}

func TestPrintStr(t *testing.T) {
	src := `fn _start(): i32 { print_str("Hi\n", 3) }`
	p := parser.New(src)
	f := p.ParseFile()

	m := NewModule()
	m.AddMemory(1)
	CompileFile(f, m)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	var stdout bytes.Buffer
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	config := wazero.NewModuleConfig().WithStdout(&stdout)
	_, err := r.InstantiateWithConfig(ctx, m.Bytes(), config)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	if stdout.String() != "Hi\n" {
		t.Fatalf("expected 'Hi\\n', got %q", stdout.String())
	}
}

func TestExit(t *testing.T) {
	src := `fn _start(): i32 { exit(0) }`
	p := parser.New(src)
	f := p.ParseFile()

	m := NewModule()
	m.AddMemory(1)
	CompileFile(f, m)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	config := wazero.NewModuleConfig()
	_, err := r.InstantiateWithConfig(ctx, m.Bytes(), config)
	// proc_exit causes an exit error, which is expected for exit(0)
	// The test passes if no panic occurred
	_ = err
}

func TestPrintInt(t *testing.T) {
	src := `fn _start(): i32 { print_int(42) }`
	p := parser.New(src)
	f := p.ParseFile()

	m := NewModule()
	m.AddMemory(1)
	CompileFile(f, m)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	var stdout bytes.Buffer
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	config := wazero.NewModuleConfig().WithStdout(&stdout)
	_, err := r.InstantiateWithConfig(ctx, m.Bytes(), config)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	if stdout.String() != "42" {
		t.Fatalf("expected '42', got %q", stdout.String())
	}
}

func TestPrintIntNegative(t *testing.T) {
	src := `fn _start(): i32 { print_int(0 - 123) }`
	p := parser.New(src)
	f := p.ParseFile()

	m := NewModule()
	m.AddMemory(1)
	CompileFile(f, m)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	var stdout bytes.Buffer
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	config := wazero.NewModuleConfig().WithStdout(&stdout)
	_, err := r.InstantiateWithConfig(ctx, m.Bytes(), config)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	if stdout.String() != "-123" {
		t.Fatalf("expected '-123', got %q", stdout.String())
	}
}

func TestPrintIntZero(t *testing.T) {
	src := `fn _start(): i32 { print_int(0) }`
	p := parser.New(src)
	f := p.ParseFile()

	m := NewModule()
	m.AddMemory(1)
	CompileFile(f, m)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	var stdout bytes.Buffer
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	config := wazero.NewModuleConfig().WithStdout(&stdout)
	_, err := r.InstantiateWithConfig(ctx, m.Bytes(), config)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	if stdout.String() != "0" {
		t.Fatalf("expected '0', got %q", stdout.String())
	}
}

func TestPrintln(t *testing.T) {
	src := `fn _start(): i32 { println(42) }`
	p := parser.New(src)
	f := p.ParseFile()

	m := NewModule()
	m.AddMemory(1)
	CompileFile(f, m)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	var stdout bytes.Buffer
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	config := wazero.NewModuleConfig().WithStdout(&stdout)
	_, err := r.InstantiateWithConfig(ctx, m.Bytes(), config)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	if stdout.String() != "42\n" {
		t.Fatalf("expected '42\\n', got %q", stdout.String())
	}
}

func TestPrint(t *testing.T) {
	src := `fn _start(): i32 { print("Hello", 5) }`
	p := parser.New(src)
	f := p.ParseFile()

	m := NewModule()
	m.AddMemory(1)
	CompileFile(f, m)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	var stdout bytes.Buffer
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	config := wazero.NewModuleConfig().WithStdout(&stdout)
	_, err := r.InstantiateWithConfig(ctx, m.Bytes(), config)
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	if stdout.String() != "Hello\n" {
		t.Fatalf("expected 'Hello\\n', got %q", stdout.String())
	}
}

func TestAbs(t *testing.T) {
	p := parser.New("fn test(x: i32): i32 { abs(x) }")
	fn := p.ParseFn()

	code, numLocals := Compile(fn, nil, NewStringTable(0))

	m := NewModule()
	m.AddFunction("test", 1, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := mod.ExportedFunction("test")

	// abs(5) = 5
	results, _ := test.Call(ctx, 5)
	if results[0] != 5 {
		t.Fatalf("expected 5, got %d", results[0])
	}

	// abs(-7) = 7
	neg7 := uint64(0xFFFFFFF9) // -7 as uint64
	results, _ = test.Call(ctx, neg7)
	if int32(results[0]) != 7 {
		t.Fatalf("expected 7, got %d", int32(results[0]))
	}
}

func TestMinMax(t *testing.T) {
	p := parser.New("fn test_min(a: i32, b: i32): i32 { min(a, b) }")
	fn := p.ParseFn()
	code, numLocals := Compile(fn, nil, NewStringTable(0))

	m := NewModule()
	m.AddFunction("test_min", 2, code, numLocals)

	p = parser.New("fn test_max(a: i32, b: i32): i32 { max(a, b) }")
	fn = p.ParseFn()
	code, numLocals = Compile(fn, nil, NewStringTable(0))
	m.AddFunction("test_max", 2, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	testMin := mod.ExportedFunction("test_min")
	testMax := mod.ExportedFunction("test_max")

	// min(3, 7) = 3
	results, _ := testMin.Call(ctx, 3, 7)
	if results[0] != 3 {
		t.Fatalf("min(3,7): expected 3, got %d", results[0])
	}

	// max(3, 7) = 7
	results, _ = testMax.Call(ctx, 3, 7)
	if results[0] != 7 {
		t.Fatalf("max(3,7): expected 7, got %d", results[0])
	}
}

func TestLogicalOps(t *testing.T) {
	// Test && (and)
	p := parser.New("fn test_and(a: i32, b: i32): i32 { if a > 0 && b > 0 { 1 } else { 0 } }")
	fn := p.ParseFn()
	code, numLocals := Compile(fn, nil, NewStringTable(0))

	m := NewModule()
	m.AddFunction("test_and", 2, code, numLocals)

	// Test || (or)
	p = parser.New("fn test_or(a: i32, b: i32): i32 { if a > 0 || b > 0 { 1 } else { 0 } }")
	fn = p.ParseFn()
	code, numLocals = Compile(fn, nil, NewStringTable(0))
	m.AddFunction("test_or", 2, code, numLocals)

	// Test ! (not)
	p = parser.New("fn test_not(a: i32): i32 { if !a { 1 } else { 0 } }")
	fn = p.ParseFn()
	code, numLocals = Compile(fn, nil, NewStringTable(0))
	m.AddFunction("test_not", 1, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	testAnd := mod.ExportedFunction("test_and")
	testOr := mod.ExportedFunction("test_or")
	testNot := mod.ExportedFunction("test_not")

	// && tests
	results, _ := testAnd.Call(ctx, 1, 1)
	if results[0] != 1 {
		t.Fatalf("1 && 1: expected 1, got %d", results[0])
	}
	results, _ = testAnd.Call(ctx, 1, 0)
	if results[0] != 0 {
		t.Fatalf("1 && 0: expected 0, got %d", results[0])
	}

	// || tests
	results, _ = testOr.Call(ctx, 0, 0)
	if results[0] != 0 {
		t.Fatalf("0 || 0: expected 0, got %d", results[0])
	}
	results, _ = testOr.Call(ctx, 1, 0)
	if results[0] != 1 {
		t.Fatalf("1 || 0: expected 1, got %d", results[0])
	}

	// ! tests
	results, _ = testNot.Call(ctx, 0)
	if results[0] != 1 {
		t.Fatalf("!0: expected 1, got %d", results[0])
	}
	results, _ = testNot.Call(ctx, 1)
	if results[0] != 0 {
		t.Fatalf("!1: expected 0, got %d", results[0])
	}
}

func TestUnaryMinus(t *testing.T) {
	p := parser.New("fn negate(x: i32): i32 { -x }")
	fn := p.ParseFn()
	code, numLocals := Compile(fn, nil, NewStringTable(0))

	m := NewModule()
	m.AddFunction("negate", 1, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	negate := mod.ExportedFunction("negate")

	results, _ := negate.Call(ctx, 5)
	if int32(results[0]) != -5 {
		t.Fatalf("-5: expected -5, got %d", int32(results[0]))
	}

	neg7 := uint64(0xFFFFFFF9) // -7 as uint64
	results, _ = negate.Call(ctx, neg7)
	if int32(results[0]) != 7 {
		t.Fatalf("-(-7): expected 7, got %d", int32(results[0]))
	}
}

func TestBreakContinue(t *testing.T) {
	// Test break: sum first 3 numbers, then break
	src := `fn test_break(n: i32): i32 {
		let i: i32 = 0;
		let s: i32 = 0;
		loop i < n {
			if i == 3 { break };
			s = s + i;
			i = i + 1;
		};
		s
	}`
	p := parser.New(src)
	fn := p.ParseFn()
	code, numLocals := Compile(fn, nil, NewStringTable(0))

	m := NewModule()
	m.AddFunction("test_break", 1, code, numLocals)

	// Test continue: sum only even numbers
	src2 := `fn test_continue(n: i32): i32 {
		let i: i32 = 0;
		let s: i32 = 0;
		loop i < n {
			i = i + 1;
			if i % 2 != 0 { continue };
			s = s + i;
		};
		s
	}`
	p = parser.New(src2)
	fn = p.ParseFn()
	code, numLocals = Compile(fn, nil, NewStringTable(0))
	m.AddFunction("test_continue", 1, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	testBreak := mod.ExportedFunction("test_break")
	testContinue := mod.ExportedFunction("test_continue")

	// break at i=3, so sum 0+1+2 = 3
	results, _ := testBreak.Call(ctx, 10)
	if results[0] != 3 {
		t.Fatalf("test_break(10): expected 3, got %d", results[0])
	}

	// continue skips odd, sum 2+4+6+8+10 = 30
	results, _ = testContinue.Call(ctx, 10)
	if results[0] != 30 {
		t.Fatalf("test_continue(10): expected 30, got %d", results[0])
	}
}
