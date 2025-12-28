package codegen

import (
	"bytes"
	"github.com/alexisbouchez/lingua/source"
)

// WASM magic number and version
var magic = []byte{0x00, 0x61, 0x73, 0x6d} // \0asm
var version = []byte{0x01, 0x00, 0x00, 0x00}

// Section IDs
const (
	SectionType     = 1
	SectionImport   = 2
	SectionFunction = 3
	SectionTable    = 4
	SectionMemory   = 5
	SectionGlobal   = 6
	SectionExport   = 7
	SectionElement  = 9
	SectionCode     = 10
	SectionData     = 11
)

// Value types
const (
	I32 = 0x7f
	I64 = 0x7e
	F32 = 0x7d
	F64 = 0x7c
)

type FuncDef struct {
	Name       string
	NumParams  int
	NumLocals  int
	Code       []byte
	ResultType byte // I32, I64, or 0 for void
}

type ImportDef struct {
	Module     string
	Name       string
	NumParams  int
	ParamTypes []byte // nil means all i32, otherwise specifies each param type
	HasResult  bool   // false for void functions like proc_exit
}

type DataSeg struct {
	Offset int
	Data   []byte
}

type GlobalDef struct {
	Name    string
	Type    byte
	Mutable bool
	Init    int64
}

type TableDef struct {
	Min      int
	Max      int // -1 means no max
	HasMax   bool
	Elements []int // function indices to populate
}

type Module struct {
	imports    []ImportDef
	funcs      []FuncDef
	funcIdx    map[string]int
	globals    []GlobalDef
	globalIdx  map[string]int
	table      *TableDef
	hasMemory  bool
	memPages   int
	data       []DataSeg
	SourceMap  *source.SourceMap // Optional source map for debugging
}

func NewModule() *Module {
	return &Module{
		funcIdx:   make(map[string]int),
		globalIdx: make(map[string]int),
	}
}

func (m *Module) AddImport(module, name string, numParams int, hasResult bool) {
	m.AddImportWithTypes(module, name, numParams, nil, hasResult)
}

func (m *Module) AddImportWithTypes(module, name string, numParams int, paramTypes []byte, hasResult bool) {
	m.funcIdx[name] = len(m.imports) + len(m.funcs)
	m.imports = append(m.imports, ImportDef{
		Module:     module,
		Name:       name,
		NumParams:  numParams,
		ParamTypes: paramTypes,
		HasResult:  hasResult,
	})
	// Re-index: imports come first, then local funcs
	for i := range m.funcs {
		m.funcIdx[m.funcs[i].Name] = len(m.imports) + i
	}
}

func (m *Module) AddFunction(name string, numParams int, code []byte, numLocals int) {
	m.AddFunctionWithResult(name, numParams, code, numLocals, I32)
}

func (m *Module) AddFunctionWithResult(name string, numParams int, code []byte, numLocals int, resultType byte) {
	m.funcIdx[name] = len(m.imports) + len(m.funcs)
	m.funcs = append(m.funcs, FuncDef{
		Name:       name,
		NumParams:  numParams,
		NumLocals:  numLocals,
		Code:       code,
		ResultType: resultType,
	})
}

func (m *Module) FuncIndex(name string) int {
	return m.funcIdx[name]
}

func (m *Module) AddMemory(pages int) {
	m.hasMemory = true
	m.memPages = pages
}

func (m *Module) AddGlobal(name string, typ byte, mutable bool, init int64) {
	m.globalIdx[name] = len(m.globals)
	m.globals = append(m.globals, GlobalDef{
		Name:    name,
		Type:    typ,
		Mutable: mutable,
		Init:    init,
	})
}

func (m *Module) GlobalIndex(name string) int {
	return m.globalIdx[name]
}

func (m *Module) AddData(offset int, data []byte) {
	m.data = append(m.data, DataSeg{Offset: offset, Data: data})
}

func (m *Module) AddTable(min, max int, hasMax bool) {
	m.table = &TableDef{Min: min, Max: max, HasMax: hasMax}
}

func (m *Module) AddTableElement(funcIdx int) {
	if m.table != nil {
		m.table.Elements = append(m.table.Elements, funcIdx)
	}
}

func (m *Module) Bytes() []byte {
	var buf bytes.Buffer
	buf.Write(magic)
	buf.Write(version)

	// Type section - types are keyed by full signature string
	type funcType struct {
		paramTypes []byte
		resultType byte // 0 for void, I32, I64, etc.
	}
	typeMap := make(map[string]int)
	var types []funcType

	makeKey := func(paramTypes []byte, resultType byte) string {
		key := string(paramTypes) + "|" + string(resultType)
		return key
	}

	for _, imp := range m.imports {
		resultType := byte(0)
		if imp.HasResult {
			resultType = I32
		}
		paramTypes := imp.ParamTypes
		if paramTypes == nil {
			// Default all params to i32
			paramTypes = make([]byte, imp.NumParams)
			for i := range paramTypes {
				paramTypes[i] = I32
			}
		}
		key := makeKey(paramTypes, resultType)
		if _, ok := typeMap[key]; !ok {
			typeMap[key] = len(types)
			types = append(types, funcType{paramTypes, resultType})
		}
	}
	for _, f := range m.funcs {
		paramTypes := make([]byte, f.NumParams)
		for i := range paramTypes {
			paramTypes[i] = I32
		}
		key := makeKey(paramTypes, f.ResultType)
		if _, ok := typeMap[key]; !ok {
			typeMap[key] = len(types)
			types = append(types, funcType{paramTypes, f.ResultType})
		}
	}

	var typeSec bytes.Buffer
	typeSec.Write(uleb128(uint64(len(types))))
	for _, t := range types {
		typeSec.WriteByte(0x60) // func type
		typeSec.Write(uleb128(uint64(len(t.paramTypes))))
		for _, pt := range t.paramTypes {
			typeSec.WriteByte(pt)
		}
		if t.resultType != 0 {
			typeSec.WriteByte(1)           // 1 result
			typeSec.WriteByte(t.resultType) // result type
		} else {
			typeSec.WriteByte(0) // 0 results (void)
		}
	}
	writeSection(&buf, SectionType, typeSec.Bytes())

	// Import section
	if len(m.imports) > 0 {
		var impSec bytes.Buffer
		impSec.Write(uleb128(uint64(len(m.imports))))
		for _, imp := range m.imports {
			impSec.Write(uleb128(uint64(len(imp.Module))))
			impSec.WriteString(imp.Module)
			impSec.Write(uleb128(uint64(len(imp.Name))))
			impSec.WriteString(imp.Name)
			impSec.WriteByte(0x00) // func import
			resultType := byte(0)
			if imp.HasResult {
				resultType = I32
			}
			paramTypes := imp.ParamTypes
			if paramTypes == nil {
				paramTypes = make([]byte, imp.NumParams)
				for i := range paramTypes {
					paramTypes[i] = I32
				}
			}
			key := makeKey(paramTypes, resultType)
			impSec.Write(uleb128(uint64(typeMap[key])))
		}
		writeSection(&buf, SectionImport, impSec.Bytes())
	}

	// Function section
	var funcSec bytes.Buffer
	funcSec.Write(uleb128(uint64(len(m.funcs))))
	for _, f := range m.funcs {
		paramTypes := make([]byte, f.NumParams)
		for i := range paramTypes {
			paramTypes[i] = I32
		}
		key := makeKey(paramTypes, f.ResultType)
		funcSec.Write(uleb128(uint64(typeMap[key])))
	}
	writeSection(&buf, SectionFunction, funcSec.Bytes())

	// Table section
	if m.table != nil {
		var tableSec bytes.Buffer
		tableSec.WriteByte(1)    // 1 table
		tableSec.WriteByte(0x70) // funcref type
		if m.table.HasMax {
			tableSec.WriteByte(0x01) // has max
			tableSec.Write(uleb128(uint64(m.table.Min)))
			tableSec.Write(uleb128(uint64(m.table.Max)))
		} else {
			tableSec.WriteByte(0x00) // no max
			tableSec.Write(uleb128(uint64(m.table.Min)))
		}
		writeSection(&buf, SectionTable, tableSec.Bytes())
	}

	// Memory section
	if m.hasMemory {
		var memSec bytes.Buffer
		memSec.WriteByte(1)                          // 1 memory
		memSec.WriteByte(0x00)                       // no max
		memSec.Write(uleb128(uint64(m.memPages)))    // initial pages
		writeSection(&buf, SectionMemory, memSec.Bytes())
	}

	// Global section
	if len(m.globals) > 0 {
		var globalSec bytes.Buffer
		globalSec.Write(uleb128(uint64(len(m.globals))))
		for _, g := range m.globals {
			globalSec.WriteByte(g.Type)
			if g.Mutable {
				globalSec.WriteByte(0x01)
			} else {
				globalSec.WriteByte(0x00)
			}
			globalSec.WriteByte(0x41) // i32.const
			globalSec.Write(sleb128(g.Init))
			globalSec.WriteByte(0x0b) // end
		}
		writeSection(&buf, SectionGlobal, globalSec.Bytes())
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
		expSec.Write(uleb128(uint64(len(m.imports) + i)))
	}
	if m.hasMemory {
		expSec.Write(uleb128(uint64(len("memory"))))
		expSec.WriteString("memory")
		expSec.WriteByte(0x02) // memory export
		expSec.WriteByte(0)    // memory index 0
	}
	writeSection(&buf, SectionExport, expSec.Bytes())

	// Element section (populates table with function references)
	if m.table != nil && len(m.table.Elements) > 0 {
		var elemSec bytes.Buffer
		elemSec.WriteByte(1)    // 1 element segment
		elemSec.WriteByte(0)    // table index 0
		elemSec.WriteByte(0x41) // i32.const
		elemSec.WriteByte(0)    // offset 0
		elemSec.WriteByte(0x0b) // end
		elemSec.Write(uleb128(uint64(len(m.table.Elements))))
		for _, funcIdx := range m.table.Elements {
			elemSec.Write(uleb128(uint64(funcIdx)))
		}
		writeSection(&buf, SectionElement, elemSec.Bytes())
	}

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

	// Data section
	if len(m.data) > 0 {
		var dataSec bytes.Buffer
		dataSec.Write(uleb128(uint64(len(m.data))))
		for _, d := range m.data {
			dataSec.WriteByte(0) // memory index 0
			dataSec.WriteByte(0x41) // i32.const
			dataSec.Write(sleb128(int64(d.Offset)))
			dataSec.WriteByte(0x0b) // end
			dataSec.Write(uleb128(uint64(len(d.Data))))
			dataSec.Write(d.Data)
		}
		writeSection(&buf, SectionData, dataSec.Bytes())
	}

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
