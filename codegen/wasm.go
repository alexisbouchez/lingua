package codegen

import "bytes"

// WASM magic number and version
var magic = []byte{0x00, 0x61, 0x73, 0x6d} // \0asm
var version = []byte{0x01, 0x00, 0x00, 0x00}

// Section IDs
const (
	SectionType     = 1
	SectionFunction = 3
	SectionExport   = 7
	SectionCode     = 10
)

// Value types
const (
	I32 = 0x7f
	I64 = 0x7e
	F32 = 0x7d
	F64 = 0x7c
)

type Module struct {
	buf bytes.Buffer
}

func NewModule() *Module {
	m := &Module{}
	m.buf.Write(magic)
	m.buf.Write(version)
	return m
}

func (m *Module) Bytes() []byte {
	return m.buf.Bytes()
}

func (m *Module) writeSection(id byte, content []byte) {
	m.buf.WriteByte(id)
	m.writeULEB128(uint64(len(content)))
	m.buf.Write(content)
}

func (m *Module) writeULEB128(v uint64) {
	for {
		b := byte(v & 0x7f)
		v >>= 7
		if v != 0 {
			b |= 0x80
		}
		m.buf.WriteByte(b)
		if v == 0 {
			break
		}
	}
}

func uleb128(v uint64) []byte {
	var buf bytes.Buffer
	for {
		b := byte(v & 0x7f)
		v >>= 7
		if v != 0 {
			b |= 0x80
		}
		buf.WriteByte(b)
		if v == 0 {
			break
		}
	}
	return buf.Bytes()
}

func sleb128(v int64) []byte {
	var buf bytes.Buffer
	for {
		b := byte(v & 0x7f)
		v >>= 7
		more := !((v == 0 && (b&0x40) == 0) || (v == -1 && (b&0x40) != 0))
		if more {
			b |= 0x80
		}
		buf.WriteByte(b)
		if !more {
			break
		}
	}
	return buf.Bytes()
}

// AddFunction adds a function that returns i32
func (m *Module) AddFunction(name string, code []byte, numLocals int) {
	// Type section: (i32, i32) -> i32
	var typeSec bytes.Buffer
	typeSec.WriteByte(1)    // 1 type
	typeSec.WriteByte(0x60) // func type
	typeSec.WriteByte(2)    // 2 params
	typeSec.WriteByte(I32)
	typeSec.WriteByte(I32)
	typeSec.WriteByte(1)   // 1 result
	typeSec.WriteByte(I32)
	m.writeSection(SectionType, typeSec.Bytes())

	// Function section
	var funcSec bytes.Buffer
	funcSec.WriteByte(1) // 1 function
	funcSec.WriteByte(0) // type index 0
	m.writeSection(SectionFunction, funcSec.Bytes())

	// Export section
	var expSec bytes.Buffer
	expSec.WriteByte(1) // 1 export
	expSec.Write(uleb128(uint64(len(name))))
	expSec.WriteString(name)
	expSec.WriteByte(0x00) // func export
	expSec.WriteByte(0)    // func index 0
	m.writeSection(SectionExport, expSec.Bytes())

	// Code section
	var codeSec bytes.Buffer
	codeSec.WriteByte(1) // 1 function body

	// Build function body with locals
	var body bytes.Buffer
	if numLocals > 0 {
		body.WriteByte(1)                         // 1 local entry
		body.Write(uleb128(uint64(numLocals)))    // count
		body.WriteByte(I32)                       // type
	} else {
		body.WriteByte(0) // 0 local entries
	}
	body.Write(code)
	body.WriteByte(0x0b) // end

	codeSec.Write(uleb128(uint64(body.Len())))
	codeSec.Write(body.Bytes())
	m.writeSection(SectionCode, codeSec.Bytes())
}
