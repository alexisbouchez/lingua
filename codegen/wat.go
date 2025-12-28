package codegen

import (
	"fmt"
	"strings"
)

// WAT generates WebAssembly Text format from a Module
func (m *Module) WAT() string {
	var b strings.Builder
	b.WriteString("(module\n")

	// Memory
	if m.hasMemory {
		b.WriteString(fmt.Sprintf("  (memory (export \"memory\") %d)\n", m.memPages))
	}

	// Globals
	for _, g := range m.globals {
		mutStr := "const"
		if g.Mutable {
			mutStr = "mut"
		}
		typeStr := "i32"
		switch g.Type {
		case I64:
			typeStr = "i64"
		case F32:
			typeStr = "f32"
		case F64:
			typeStr = "f64"
		}
		b.WriteString(fmt.Sprintf("  (global $%s (%s %s) (%s.const %d))\n", g.Name, mutStr, typeStr, typeStr, g.Init))
	}

	// Table
	if m.table != nil {
		if m.table.HasMax {
			b.WriteString(fmt.Sprintf("  (table %d %d funcref)\n", m.table.Min, m.table.Max))
		} else {
			b.WriteString(fmt.Sprintf("  (table %d funcref)\n", m.table.Min))
		}
	}

	// Imports
	for _, imp := range m.imports {
		b.WriteString(fmt.Sprintf("  (import \"%s\" \"%s\" (func $%s", imp.Module, imp.Name, imp.Name))
		if imp.NumParams > 0 {
			b.WriteString(" (param")
			for i := 0; i < imp.NumParams; i++ {
				b.WriteString(" i32")
			}
			b.WriteString(")")
		}
		b.WriteString(" (result i32)")
		b.WriteString("))\n")
	}

	// Functions
	for _, fn := range m.funcs {
		b.WriteString(fmt.Sprintf("  (func $%s", fn.Name))
		if fn.NumParams > 0 {
			for i := 0; i < fn.NumParams; i++ {
				b.WriteString(fmt.Sprintf(" (param $p%d i32)", i))
			}
		}
		b.WriteString(" (result i32)")
		if fn.NumLocals > 0 {
			b.WriteString("\n    (local")
			for i := 0; i < fn.NumLocals; i++ {
				b.WriteString(" i32")
			}
			b.WriteString(")")
		}
		b.WriteString("\n")
		b.WriteString(disassemble(fn.Code, 4))
		b.WriteString("  )\n")

		// Export
		if fn.Name[0] != '_' {
			b.WriteString(fmt.Sprintf("  (export \"%s\" (func $%s))\n", fn.Name, fn.Name))
		}
	}

	// Element section
	if m.table != nil && len(m.table.Elements) > 0 {
		b.WriteString("  (elem (i32.const 0)")
		for _, funcIdx := range m.table.Elements {
			b.WriteString(fmt.Sprintf(" %d", funcIdx))
		}
		b.WriteString(")\n")
	}

	// Data sections
	for _, d := range m.data {
		b.WriteString(fmt.Sprintf("  (data (i32.const %d) \"", d.Offset))
		for _, c := range d.Data {
			if c >= 32 && c < 127 && c != '"' && c != '\\' {
				b.WriteByte(c)
			} else {
				b.WriteString(fmt.Sprintf("\\%02x", c))
			}
		}
		b.WriteString("\")\n")
	}

	b.WriteString(")\n")
	return b.String()
}

func disassemble(code []byte, indent int) string {
	var b strings.Builder
	prefix := strings.Repeat(" ", indent)
	i := 0

	for i < len(code) {
		b.WriteString(prefix)
		op := code[i]
		i++

		switch op {
		case 0x00:
			b.WriteString("unreachable")
		case 0x01:
			b.WriteString("nop")
		case 0x02:
			blockType := code[i]
			i++
			b.WriteString(fmt.Sprintf("block %s", blockTypeStr(blockType)))
		case 0x03:
			blockType := code[i]
			i++
			b.WriteString(fmt.Sprintf("loop %s", blockTypeStr(blockType)))
		case 0x04:
			blockType := code[i]
			i++
			b.WriteString(fmt.Sprintf("if %s", blockTypeStr(blockType)))
		case 0x05:
			b.WriteString("else")
		case 0x0b:
			b.WriteString("end")
		case 0x0c:
			label := code[i]
			i++
			b.WriteString(fmt.Sprintf("br %d", label))
		case 0x0d:
			label := code[i]
			i++
			b.WriteString(fmt.Sprintf("br_if %d", label))
		case 0x0f:
			b.WriteString("return")
		case 0x10:
			idx := code[i]
			i++
			b.WriteString(fmt.Sprintf("call %d", idx))
		case 0x11:
			typeIdx := code[i]
			i++
			tableIdx := code[i]
			i++
			b.WriteString(fmt.Sprintf("call_indirect (type %d) (table %d)", typeIdx, tableIdx))
		case 0x1a:
			b.WriteString("drop")
		case 0x20:
			idx := code[i]
			i++
			b.WriteString(fmt.Sprintf("local.get %d", idx))
		case 0x21:
			idx := code[i]
			i++
			b.WriteString(fmt.Sprintf("local.set %d", idx))
		case 0x23:
			idx := code[i]
			i++
			b.WriteString(fmt.Sprintf("global.get %d", idx))
		case 0x24:
			idx := code[i]
			i++
			b.WriteString(fmt.Sprintf("global.set %d", idx))
		case 0x28:
			align := code[i]
			i++
			offset := code[i]
			i++
			b.WriteString(fmt.Sprintf("i32.load align=%d offset=%d", 1<<align, offset))
		case 0x36:
			align := code[i]
			i++
			offset := code[i]
			i++
			b.WriteString(fmt.Sprintf("i32.store align=%d offset=%d", 1<<align, offset))
		case 0x3a:
			_ = code[i]
			i++
			_ = code[i]
			i++
			b.WriteString("i32.store8")
		case 0x41:
			val, n := decodeSLEB128(code[i:])
			i += n
			b.WriteString(fmt.Sprintf("i32.const %d", val))
		case 0x45:
			b.WriteString("i32.eqz")
		case 0x46:
			b.WriteString("i32.eq")
		case 0x47:
			b.WriteString("i32.ne")
		case 0x48:
			b.WriteString("i32.lt_s")
		case 0x4a:
			b.WriteString("i32.gt_s")
		case 0x4c:
			b.WriteString("i32.le_s")
		case 0x4e:
			b.WriteString("i32.ge_s")
		case 0x6a:
			b.WriteString("i32.add")
		case 0x6b:
			b.WriteString("i32.sub")
		case 0x6c:
			b.WriteString("i32.mul")
		case 0x6d:
			b.WriteString("i32.div_s")
		case 0x6f:
			b.WriteString("i32.rem_s")
		case 0x71:
			b.WriteString("i32.and")
		case 0x72:
			b.WriteString("i32.or")
		case 0x73:
			b.WriteString("i32.xor")
		case 0x74:
			b.WriteString("i32.shl")
		case 0x75:
			b.WriteString("i32.shr_s")
		default:
			b.WriteString(fmt.Sprintf("unknown_0x%02x", op))
		}
		b.WriteString("\n")
	}

	return b.String()
}

func blockTypeStr(t byte) string {
	switch t {
	case 0x40:
		return "(result)"
	case 0x7f:
		return "(result i32)"
	default:
		return fmt.Sprintf("(type %d)", t)
	}
}

func decodeSLEB128(data []byte) (int64, int) {
	var result int64
	var shift uint
	var i int
	for {
		b := data[i]
		i++
		result |= int64(b&0x7f) << shift
		shift += 7
		if b&0x80 == 0 {
			if shift < 64 && (b&0x40) != 0 {
				result |= -(1 << shift)
			}
			break
		}
	}
	return result, i
}
