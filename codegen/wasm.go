package codegen

import "bytes"

// WASM magic number and version
var magic = []byte{0x00, 0x61, 0x73, 0x6d} // \0asm
var version = []byte{0x01, 0x00, 0x00, 0x00}

// Section IDs
const (
	SectionType     = 1
	SectionFunction = 3
	SectionMemory   = 5
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

type FuncDef struct {
	Name      string
	NumParams int
	NumLocals int
	Code      []byte
}

type Module struct {
	funcs     []FuncDef
	funcIdx   map[string]int
	hasMemory bool
	memPages  int
}

func NewModule() *Module {
	return &Module{
		funcIdx: make(map[string]int),
	}
}

func (m *Module) AddFunction(name string, numParams int, code []byte, numLocals int) {
	m.funcIdx[name] = len(m.funcs)
	m.funcs = append(m.funcs, FuncDef{
		Name:      name,
		NumParams: numParams,
		NumLocals: numLocals,
		Code:      code,
	})
}

func (m *Module) FuncIndex(name string) int {
	return m.funcIdx[name]
}

func (m *Module) AddMemory(pages int) {
	m.hasMemory = true
	m.memPages = pages
}

func (m *Module) Bytes() []byte {
	var buf bytes.Buffer
	buf.Write(magic)
	buf.Write(version)

	// Type section - one type per function based on param count
	typeMap := make(map[int]int) // numParams -> typeIdx
	var types []int
	for _, f := range m.funcs {
		if _, ok := typeMap[f.NumParams]; !ok {
			typeMap[f.NumParams] = len(types)
			types = append(types, f.NumParams)
		}
	}

	var typeSec bytes.Buffer
	typeSec.Write(uleb128(uint64(len(types))))
	for _, numParams := range types {
		typeSec.WriteByte(0x60) // func type
		typeSec.Write(uleb128(uint64(numParams)))
		for i := 0; i < numParams; i++ {
			typeSec.WriteByte(I32)
		}
		typeSec.WriteByte(1)   // 1 result
		typeSec.WriteByte(I32) // i32
	}
	writeSection(&buf, SectionType, typeSec.Bytes())

	// Function section
	var funcSec bytes.Buffer
	funcSec.Write(uleb128(uint64(len(m.funcs))))
	for _, f := range m.funcs {
		funcSec.Write(uleb128(uint64(typeMap[f.NumParams])))
	}
	writeSection(&buf, SectionFunction, funcSec.Bytes())

	// Memory section
	if m.hasMemory {
		var memSec bytes.Buffer
		memSec.WriteByte(1)                          // 1 memory
		memSec.WriteByte(0x00)                       // no max
		memSec.Write(uleb128(uint64(m.memPages)))    // initial pages
		writeSection(&buf, SectionMemory, memSec.Bytes())
	}

	// Export section
	numExports := len(m.funcs)
	if m.hasMemory {
		numExports++
	}
	var expSec bytes.Buffer
	expSec.Write(uleb128(uint64(numExports)))
	for i, f := range m.funcs {
		expSec.Write(uleb128(uint64(len(f.Name))))
		expSec.WriteString(f.Name)
		expSec.WriteByte(0x00) // func export
		expSec.Write(uleb128(uint64(i)))
	}
	if m.hasMemory {
		expSec.Write(uleb128(uint64(len("memory"))))
		expSec.WriteString("memory")
		expSec.WriteByte(0x02) // memory export
		expSec.WriteByte(0)    // memory index 0
	}
	writeSection(&buf, SectionExport, expSec.Bytes())

	// Code section
	var codeSec bytes.Buffer
	codeSec.Write(uleb128(uint64(len(m.funcs))))
	for _, f := range m.funcs {
		var body bytes.Buffer
		if f.NumLocals > 0 {
			body.WriteByte(1)
			body.Write(uleb128(uint64(f.NumLocals)))
			body.WriteByte(I32)
		} else {
			body.WriteByte(0)
		}
		body.Write(f.Code)
		body.WriteByte(0x0b) // end

		codeSec.Write(uleb128(uint64(body.Len())))
		codeSec.Write(body.Bytes())
	}
	writeSection(&buf, SectionCode, codeSec.Bytes())

	return buf.Bytes()
}

func writeSection(buf *bytes.Buffer, id byte, content []byte) {
	buf.WriteByte(id)
	buf.Write(uleb128(uint64(len(content))))
	buf.Write(content)
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
