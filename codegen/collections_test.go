package codegen

import (
	"context"
	"testing"

	"github.com/alexisbouchez/lingua/parser"
	"github.com/tetratelabs/wazero"
	"github.com/tetratelabs/wazero/imports/wasi_snapshot_preview1"
)

// Helper function to set up module with heap pointer for collection tests
func setupCollectionModule() (*Module, map[string]int) {
	m := NewModule()
	// Add memory (1 page = 64KB)
	m.AddMemory(1)
	// Add heap pointer global starting at 4096 (same as CompileFile)
	m.AddGlobal("__heap_ptr", I32, true, 4096)
	globalIdx := map[string]int{"__heap_ptr": 0}
	return m, globalIdx
}

// ============================================================================
// LIST TESTS
// ============================================================================

func TestListNew(t *testing.T) {
	// list_new() should return a pointer to a new list
	src := `fn test(): i32 {
		let l: i32 = list_new();
		l
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	m, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	m.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := mod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	// list_new should return a non-zero pointer
	if results[0] == 0 {
		t.Fatalf("expected non-zero pointer, got 0")
	}
}

func TestListPushAndLen(t *testing.T) {
	// list_push adds elements, list_len returns count
	src := `fn test(): i32 {
		let l: i32 = list_new();
		list_push(l, 10);
		list_push(l, 20);
		list_push(l, 30);
		list_len(l)
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	m, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	m.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := mod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] != 3 {
		t.Fatalf("expected len=3, got %d", results[0])
	}
}

func TestListGet(t *testing.T) {
	// list_get retrieves elements by index
	src := `fn test(): i32 {
		let l: i32 = list_new();
		list_push(l, 100);
		list_push(l, 200);
		list_push(l, 300);
		list_get(l, 1)
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	m, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	m.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := mod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] != 200 {
		t.Fatalf("expected 200, got %d", results[0])
	}
}

func TestListSet(t *testing.T) {
	// list_set modifies elements by index
	src := `fn test(): i32 {
		let l: i32 = list_new();
		list_push(l, 10);
		list_push(l, 20);
		list_set(l, 0, 99);
		list_get(l, 0)
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	m, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	m.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := mod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] != 99 {
		t.Fatalf("expected 99, got %d", results[0])
	}
}

func TestListPop(t *testing.T) {
	// list_pop removes and returns the last element
	src := `fn test(): i32 {
		let l: i32 = list_new();
		list_push(l, 10);
		list_push(l, 20);
		list_push(l, 30);
		let popped: i32 = list_pop(l);
		popped
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	m, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	m.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := mod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] != 30 {
		t.Fatalf("expected 30, got %d", results[0])
	}
}

func TestListPopReducesLen(t *testing.T) {
	// list_pop should reduce the length
	src := `fn test(): i32 {
		let l: i32 = list_new();
		list_push(l, 10);
		list_push(l, 20);
		list_pop(l);
		list_len(l)
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	m, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	m.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := mod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] != 1 {
		t.Fatalf("expected len=1, got %d", results[0])
	}
}

// ============================================================================
// MAP TESTS
// ============================================================================

func TestMapNew(t *testing.T) {
	// map_new() should return a pointer to a new map
	src := `fn test(): i32 {
		let m: i32 = map_new();
		m
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	mod, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	mod.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	wasmMod, err := r.Instantiate(ctx, mod.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := wasmMod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] == 0 {
		t.Fatalf("expected non-zero pointer, got 0")
	}
}

func TestMapSetAndGet(t *testing.T) {
	// map_set and map_get for key-value storage
	src := `fn test(): i32 {
		let m: i32 = map_new();
		map_set(m, 42, 100);
		map_get(m, 42)
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	mod, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	mod.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	wasmMod, err := r.Instantiate(ctx, mod.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := wasmMod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] != 100 {
		t.Fatalf("expected 100, got %d", results[0])
	}
}

func TestMapHas(t *testing.T) {
	// map_has checks if a key exists
	src := `fn test(): i32 {
		let m: i32 = map_new();
		map_set(m, 5, 50);
		map_has(m, 5)
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	mod, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	mod.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	wasmMod, err := r.Instantiate(ctx, mod.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := wasmMod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] != 1 {
		t.Fatalf("expected 1 (true), got %d", results[0])
	}
}

func TestMapHasNotFound(t *testing.T) {
	// map_has returns 0 for missing key
	src := `fn test(): i32 {
		let m: i32 = map_new();
		map_set(m, 5, 50);
		map_has(m, 99)
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	mod, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	mod.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	wasmMod, err := r.Instantiate(ctx, mod.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := wasmMod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] != 0 {
		t.Fatalf("expected 0 (false), got %d", results[0])
	}
}

func TestMapMultipleKeys(t *testing.T) {
	// Multiple key-value pairs in map
	src := `fn test(): i32 {
		let m: i32 = map_new();
		map_set(m, 1, 10);
		map_set(m, 2, 20);
		map_set(m, 3, 30);
		map_get(m, 1) + map_get(m, 2) + map_get(m, 3)
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	mod, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	mod.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	wasmMod, err := r.Instantiate(ctx, mod.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := wasmMod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] != 60 {
		t.Fatalf("expected 60, got %d", results[0])
	}
}

func TestMapUpdateKey(t *testing.T) {
	// Setting the same key again should update the value
	src := `fn test(): i32 {
		let m: i32 = map_new();
		map_set(m, 5, 100);
		map_set(m, 5, 999);
		map_get(m, 5)
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	mod, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	mod.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	wasmMod, err := r.Instantiate(ctx, mod.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := wasmMod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] != 999 {
		t.Fatalf("expected 999, got %d", results[0])
	}
}

// ============================================================================
// SET TESTS
// ============================================================================

func TestSetNew(t *testing.T) {
	// set_new() should return a pointer to a new set
	src := `fn test(): i32 {
		let s: i32 = set_new();
		s
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	m, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	m.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := mod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] == 0 {
		t.Fatalf("expected non-zero pointer, got 0")
	}
}

func TestSetAddAndHas(t *testing.T) {
	// set_add adds values, set_has checks membership
	src := `fn test(): i32 {
		let s: i32 = set_new();
		set_add(s, 42);
		set_has(s, 42)
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	m, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	m.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := mod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] != 1 {
		t.Fatalf("expected 1 (true), got %d", results[0])
	}
}

func TestSetHasNotFound(t *testing.T) {
	// set_has returns 0 for non-existent values
	src := `fn test(): i32 {
		let s: i32 = set_new();
		set_add(s, 10);
		set_has(s, 99)
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	m, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	m.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := mod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] != 0 {
		t.Fatalf("expected 0 (false), got %d", results[0])
	}
}

func TestSetRemove(t *testing.T) {
	// set_remove removes a value from the set
	src := `fn test(): i32 {
		let s: i32 = set_new();
		set_add(s, 42);
		set_remove(s, 42);
		set_has(s, 42)
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	m, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	m.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := mod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	if results[0] != 0 {
		t.Fatalf("expected 0 (false after remove), got %d", results[0])
	}
}

func TestSetLen(t *testing.T) {
	// set_len returns the number of unique elements
	src := `fn test(): i32 {
		let s: i32 = set_new();
		set_add(s, 1);
		set_add(s, 2);
		set_add(s, 3);
		set_add(s, 2);
		set_len(s)
	}`
	p := parser.New(src)
	fn := p.ParseFn()

	m, globalIdx := setupCollectionModule()
	code, numLocals := Compile(fn, nil, globalIdx, nil, NewStringTable(1024), false)
	m.AddFunction("test", 0, code, numLocals)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)
	wasi_snapshot_preview1.MustInstantiate(ctx, r)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate: %v", err)
	}

	test := mod.ExportedFunction("test")
	results, err := test.Call(ctx)
	if err != nil {
		t.Fatalf("call: %v", err)
	}

	// 3 unique values (duplicate 2 should not increase count)
	if results[0] != 3 {
		t.Fatalf("expected 3, got %d", results[0])
	}
}
