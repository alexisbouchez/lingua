package codegen

import (
	"bytes"
	"context"
	"testing"

	"github.com/tetratelabs/wazero"
)

func TestModuleHeader(t *testing.T) {
	m := NewModule()
	b := m.Bytes()

	// Check magic number
	if !bytes.Equal(b[:4], []byte{0x00, 0x61, 0x73, 0x6d}) {
		t.Fatalf("wrong magic: %x", b[:4])
	}

	// Check version
	if !bytes.Equal(b[4:8], []byte{0x01, 0x00, 0x00, 0x00}) {
		t.Fatalf("wrong version: %x", b[4:8])
	}
}

func TestAddFunction(t *testing.T) {
	m := NewModule()

	// i32.add of two params: local.get 0, local.get 1, i32.add
	code := []byte{
		0x20, 0x00, // local.get 0
		0x20, 0x01, // local.get 1
		0x6a,       // i32.add
	}
	m.AddFunction("add", code, 0)

	b := m.Bytes()
	if len(b) < 20 {
		t.Fatalf("module too short: %d bytes", len(b))
	}

	// Verify sections exist by checking section IDs appear
	hasType := bytes.Contains(b, []byte{SectionType})
	hasFunc := bytes.Contains(b, []byte{SectionFunction})
	hasExport := bytes.Contains(b, []byte{SectionExport})
	hasCode := bytes.Contains(b, []byte{SectionCode})

	if !hasType || !hasFunc || !hasExport || !hasCode {
		t.Fatalf("missing sections: type=%v func=%v export=%v code=%v",
			hasType, hasFunc, hasExport, hasCode)
	}
}

func TestWazeroExecution(t *testing.T) {
	m := NewModule()

	// add(a, b) = a + b
	code := []byte{
		0x20, 0x00, // local.get 0
		0x20, 0x01, // local.get 1
		0x6a,       // i32.add
	}
	m.AddFunction("add", code, 0)

	ctx := context.Background()
	r := wazero.NewRuntime(ctx)
	defer r.Close(ctx)

	mod, err := r.Instantiate(ctx, m.Bytes())
	if err != nil {
		t.Fatalf("instantiate failed: %v", err)
	}

	add := mod.ExportedFunction("add")
	if add == nil {
		t.Fatal("add not exported")
	}

	results, err := add.Call(ctx, 3, 5)
	if err != nil {
		t.Fatalf("call failed: %v", err)
	}

	if len(results) != 1 || results[0] != 8 {
		t.Fatalf("expected 8, got %v", results)
	}
}
