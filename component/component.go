package component

import (
	"encoding/binary"
)

// Component represents a WebAssembly Component
// Components wrap core modules and provide typed interfaces
type Component struct {
	// Embedded core module
	coreModule []byte

	// Component-level imports
	imports []ComponentImport

	// Component-level exports
	exports []ComponentExport

	// Type definitions
	types []ComponentType
}

// Types returns the component's type definitions
func (c *Component) Types() []ComponentType {
	return c.types
}

// Imports returns the component's imports
func (c *Component) Imports() []ComponentImport {
	return c.imports
}

// Exports returns the component's exports
func (c *Component) Exports() []ComponentExport {
	return c.exports
}

// ComponentImport represents a component import
type ComponentImport struct {
	Name      string
	Interface string // e.g., "wasi:http/outgoing-handler@0.2.0"
	Type      ComponentType // The imported type (function, interface, etc.)
}

// ComponentExport represents a component export
type ComponentExport struct {
	Name string
	Kind ExportKind
	Idx  int
	Type ComponentType // The exported type (function, interface, etc.)
}

// ExportKind represents the kind of export
type ExportKind byte

const (
	ExportFunc ExportKind = iota
	ExportType
	ExportComponent
	ExportInstance
)

// ComponentType represents a component-level type
// This includes function types, interface types, etc.
type ComponentType interface {
	isComponentType()
	TypeID() byte // Returns the type identifier
}

// InterfaceType represents a component interface type
// Interfaces define sets of functions that can be imported/exported
type InterfaceType struct {
	Name      string      // Interface name
	Functions []FuncType  // Functions in the interface
	Version   string      // Optional version
}

func (InterfaceType) isComponentType() {}
func (InterfaceType) TypeID() byte     { return 0x02 } // Interface type ID

// FuncType represents a component function type
type FuncType struct {
	Params  []NamedType
	Results []NamedType
}

func (FuncType) isComponentType() {}
func (FuncType) TypeID() byte      { return 0x01 } // Function type ID

// NamedType represents a named parameter or result
type NamedType struct {
	Name string
	Type ValType
}

// ValType represents component value types
type ValType byte

const (
	ValBool ValType = iota
	ValS8
	ValU8
	ValS16
	ValU16
	ValS32
	ValU32
	ValS64
	ValU64
	ValF32
	ValF64
	ValChar
	ValString
	ValList
	ValRecord
	ValVariant
	ValOption
	ValResult
)

// AddInterface adds an interface type to the component
func (c *Component) AddInterface(iface InterfaceType) {
	c.types = append(c.types, iface)
}

// AddInterfaceImport adds an interface import to the component
func (c *Component) AddInterfaceImport(name string, iface InterfaceType) {
	c.imports = append(c.imports, ComponentImport{
		Name:      name,
		Interface: iface.Name,
		Type:      iface,
	})
}

// AddInterfaceExport adds an interface export to the component
func (c *Component) AddInterfaceExport(name string, iface InterfaceType) {
	c.exports = append(c.exports, ComponentExport{
		Name: name,
		Kind: ExportType,
		Type: iface,
	})
}

// Component binary format constants
const (
	// Magic number (same as core WASM)
	Magic = 0x6D736100 // "\0asm" in little-endian

	// Component layer byte
	LayerComponent = 0x01
	LayerModule    = 0x00

	// Component version (draft)
	VersionComponent = 0x0d00

	// Section IDs
	SectionCustom       = 0x00
	SectionCoreModule   = 0x01
	SectionCoreInstance = 0x02
	SectionCoreType     = 0x03
	SectionComponent    = 0x04
	SectionInstance     = 0x05
	SectionAlias        = 0x06
	SectionType         = 0x07
	SectionCanon        = 0x08
	SectionStart        = 0x09
	SectionImport       = 0x0a
	SectionExport       = 0x0b
)

// New creates a new Component
func New() *Component {
	return &Component{}
}

// SetCoreModule sets the embedded core WASM module
func (c *Component) SetCoreModule(module []byte) {
	c.coreModule = module
}

// AddImport adds a component import
func (c *Component) AddImport(name, iface string) {
	c.imports = append(c.imports, ComponentImport{
		Name:      name,
		Interface: iface,
	})
}

// AddExport adds a component export
func (c *Component) AddExport(name string, kind ExportKind, idx int) {
	c.exports = append(c.exports, ComponentExport{
		Name: name,
		Kind: kind,
		Idx:  idx,
	})
}

// Bytes generates the component binary
func (c *Component) Bytes() []byte {
	var buf []byte

	// Magic number
	buf = append(buf, 0x00, 0x61, 0x73, 0x6d)

	// Layer byte (component = 0x01)
	buf = append(buf, LayerComponent)

	// Version (0x0d 0x00 for component model draft)
	buf = append(buf, 0x0d, 0x00)

	// Padding byte
	buf = append(buf, 0x00)

	// Core module section (embeds the core WASM module)
	if len(c.coreModule) > 0 {
		buf = append(buf, c.encodeCoreModuleSection()...)
	}

	return buf
}

// encodeCoreModuleSection encodes the core module as a component section
func (c *Component) encodeCoreModuleSection() []byte {
	var section []byte

	// Section ID
	section = append(section, SectionCoreModule)

	// Section size (module bytes + length prefix)
	moduleWithLen := c.encodeBytes(c.coreModule)
	section = append(section, c.encodeULEB128(uint32(len(moduleWithLen)))...)
	section = append(section, moduleWithLen...)

	return section
}

// encodeBytes encodes a byte vector with length prefix
func (c *Component) encodeBytes(b []byte) []byte {
	var result []byte
	result = append(result, c.encodeULEB128(uint32(len(b)))...)
	result = append(result, b...)
	return result
}

// encodeULEB128 encodes an unsigned integer in LEB128 format
func (c *Component) encodeULEB128(n uint32) []byte {
	var result []byte
	for {
		b := byte(n & 0x7f)
		n >>= 7
		if n != 0 {
			b |= 0x80
		}
		result = append(result, b)
		if n == 0 {
			break
		}
	}
	return result
}

// encodeSLEB128 encodes a signed integer in LEB128 format
func (c *Component) encodeSLEB128(n int32) []byte {
	var result []byte
	for {
		b := byte(n & 0x7f)
		n >>= 7
		if (n == 0 && (b&0x40) == 0) || (n == -1 && (b&0x40) != 0) {
			result = append(result, b)
			break
		}
		result = append(result, b|0x80)
	}
	return result
}

// encodeString encodes a string with length prefix
func (c *Component) encodeString(s string) []byte {
	var result []byte
	result = append(result, c.encodeULEB128(uint32(len(s)))...)
	result = append(result, []byte(s)...)
	return result
}

// Helper to suppress unused import warning
var _ = binary.LittleEndian
