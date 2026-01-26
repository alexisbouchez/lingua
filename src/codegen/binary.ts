/**
 * WASM Binary Encoding Utilities
 *
 * Implements LEB128 encoding and other binary format utilities
 * for generating valid WASM binaries.
 */

// =============================================================================
// BINARY WRITER
// =============================================================================

export class BinaryWriter {
  private buffer: number[] = [];

  // Get the final bytes
  toBytes(): Uint8Array {
    return new Uint8Array(this.buffer);
  }

  // Get current length
  get length(): number {
    return this.buffer.length;
  }

  // Write raw bytes
  writeBytes(bytes: Uint8Array | number[]): void {
    for (const b of bytes) {
      this.buffer.push(b);
    }
  }

  // Write a single byte
  writeByte(byte: number): void {
    this.buffer.push(byte & 0xff);
  }

  // Write unsigned LEB128
  writeU32(value: number): void {
    do {
      let byte = value & 0x7f;
      value >>>= 7;
      if (value !== 0) {
        byte |= 0x80;
      }
      this.buffer.push(byte);
    } while (value !== 0);
  }

  // Write signed LEB128 (32-bit)
  writeI32(value: number): void {
    let more = true;
    while (more) {
      let byte = value & 0x7f;
      value >>= 7;

      // Check if we need more bytes
      const signBit = (byte & 0x40) !== 0;
      if ((value === 0 && !signBit) || (value === -1 && signBit)) {
        more = false;
      } else {
        byte |= 0x80;
      }
      this.buffer.push(byte);
    }
  }

  // Write signed LEB128 (64-bit)
  writeI64(value: bigint): void {
    let more = true;
    while (more) {
      let byte = Number(value & 0x7fn);
      value >>= 7n;

      const signBit = (byte & 0x40) !== 0;
      if ((value === 0n && !signBit) || (value === -1n && signBit)) {
        more = false;
      } else {
        byte |= 0x80;
      }
      this.buffer.push(byte);
    }
  }

  // Write 32-bit float (IEEE 754)
  writeF32(value: number): void {
    const buf = new ArrayBuffer(4);
    new Float32Array(buf)[0] = value;
    const bytes = new Uint8Array(buf);
    for (let i = 0; i < 4; i++) {
      this.buffer.push(bytes[i]);
    }
  }

  // Write 64-bit float (IEEE 754)
  writeF64(value: number): void {
    const buf = new ArrayBuffer(8);
    new Float64Array(buf)[0] = value;
    const bytes = new Uint8Array(buf);
    for (let i = 0; i < 8; i++) {
      this.buffer.push(bytes[i]);
    }
  }

  // Write a UTF-8 string with length prefix
  writeName(name: string): void {
    const encoder = new TextEncoder();
    const bytes = encoder.encode(name);
    this.writeU32(bytes.length);
    this.writeBytes(bytes);
  }

  // Write a vector with length prefix
  writeVec<T>(items: T[], writeItem: (item: T) => void): void {
    this.writeU32(items.length);
    for (const item of items) {
      writeItem(item);
    }
  }
}

// =============================================================================
// WASM CONSTANTS
// =============================================================================

// WASM magic number and version
export const WASM_MAGIC = new Uint8Array([0x00, 0x61, 0x73, 0x6d]); // \0asm
export const WASM_VERSION = new Uint8Array([0x01, 0x00, 0x00, 0x00]); // version 1

// Section IDs
export const enum SectionId {
  Custom = 0,
  Type = 1,
  Import = 2,
  Function = 3,
  Table = 4,
  Memory = 5,
  Global = 6,
  Export = 7,
  Start = 8,
  Element = 9,
  Code = 10,
  Data = 11,
  DataCount = 12,
}

// Value types
export const enum ValType {
  I32 = 0x7f,
  I64 = 0x7e,
  F32 = 0x7d,
  F64 = 0x7c,
  V128 = 0x7b,
  FuncRef = 0x70,
  ExternRef = 0x6f,
}

// Block types
export const BLOCK_TYPE_VOID = 0x40;

// Function type constructor
export const FUNC_TYPE = 0x60;

// Export kinds
export const enum ExportKind {
  Func = 0x00,
  Table = 0x01,
  Memory = 0x02,
  Global = 0x03,
}

// Limits flags
export const enum LimitsFlags {
  NoMax = 0x00,
  HasMax = 0x01,
}

// =============================================================================
// WASM OPCODES
// =============================================================================

export const enum Op {
  // Control flow
  Unreachable = 0x00,
  Nop = 0x01,
  Block = 0x02,
  Loop = 0x03,
  If = 0x04,
  Else = 0x05,
  End = 0x0b,
  Br = 0x0c,
  BrIf = 0x0d,
  BrTable = 0x0e,
  Return = 0x0f,
  Call = 0x10,
  CallIndirect = 0x11,

  // Parametric
  Drop = 0x1a,
  Select = 0x1b,

  // Variable
  LocalGet = 0x20,
  LocalSet = 0x21,
  LocalTee = 0x22,
  GlobalGet = 0x23,
  GlobalSet = 0x24,

  // Memory
  I32Load = 0x28,
  I64Load = 0x29,
  F32Load = 0x2a,
  F64Load = 0x2b,
  I32Load8S = 0x2c,
  I32Load8U = 0x2d,
  I32Load16S = 0x2e,
  I32Load16U = 0x2f,
  I64Load8S = 0x30,
  I64Load8U = 0x31,
  I64Load16S = 0x32,
  I64Load16U = 0x33,
  I64Load32S = 0x34,
  I64Load32U = 0x35,
  I32Store = 0x36,
  I64Store = 0x37,
  F32Store = 0x38,
  F64Store = 0x39,
  I32Store8 = 0x3a,
  I32Store16 = 0x3b,
  I64Store8 = 0x3c,
  I64Store16 = 0x3d,
  I64Store32 = 0x3e,
  MemorySize = 0x3f,
  MemoryGrow = 0x40,

  // Constants
  I32Const = 0x41,
  I64Const = 0x42,
  F32Const = 0x43,
  F64Const = 0x44,

  // Comparison (i32)
  I32Eqz = 0x45,
  I32Eq = 0x46,
  I32Ne = 0x47,
  I32LtS = 0x48,
  I32LtU = 0x49,
  I32GtS = 0x4a,
  I32GtU = 0x4b,
  I32LeS = 0x4c,
  I32LeU = 0x4d,
  I32GeS = 0x4e,
  I32GeU = 0x4f,

  // Comparison (i64)
  I64Eqz = 0x50,
  I64Eq = 0x51,
  I64Ne = 0x52,
  I64LtS = 0x53,
  I64LtU = 0x54,
  I64GtS = 0x55,
  I64GtU = 0x56,
  I64LeS = 0x57,
  I64LeU = 0x58,
  I64GeS = 0x59,
  I64GeU = 0x5a,

  // Comparison (f32)
  F32Eq = 0x5b,
  F32Ne = 0x5c,
  F32Lt = 0x5d,
  F32Gt = 0x5e,
  F32Le = 0x5f,
  F32Ge = 0x60,

  // Comparison (f64)
  F64Eq = 0x61,
  F64Ne = 0x62,
  F64Lt = 0x63,
  F64Gt = 0x64,
  F64Le = 0x65,
  F64Ge = 0x66,

  // Numeric (i32)
  I32Clz = 0x67,
  I32Ctz = 0x68,
  I32Popcnt = 0x69,
  I32Add = 0x6a,
  I32Sub = 0x6b,
  I32Mul = 0x6c,
  I32DivS = 0x6d,
  I32DivU = 0x6e,
  I32RemS = 0x6f,
  I32RemU = 0x70,
  I32And = 0x71,
  I32Or = 0x72,
  I32Xor = 0x73,
  I32Shl = 0x74,
  I32ShrS = 0x75,
  I32ShrU = 0x76,
  I32Rotl = 0x77,
  I32Rotr = 0x78,

  // Numeric (i64)
  I64Clz = 0x79,
  I64Ctz = 0x7a,
  I64Popcnt = 0x7b,
  I64Add = 0x7c,
  I64Sub = 0x7d,
  I64Mul = 0x7e,
  I64DivS = 0x7f,
  I64DivU = 0x80,
  I64RemS = 0x81,
  I64RemU = 0x82,
  I64And = 0x83,
  I64Or = 0x84,
  I64Xor = 0x85,
  I64Shl = 0x86,
  I64ShrS = 0x87,
  I64ShrU = 0x88,
  I64Rotl = 0x89,
  I64Rotr = 0x8a,

  // Numeric (f32)
  F32Abs = 0x8b,
  F32Neg = 0x8c,
  F32Ceil = 0x8d,
  F32Floor = 0x8e,
  F32Trunc = 0x8f,
  F32Nearest = 0x90,
  F32Sqrt = 0x91,
  F32Add = 0x92,
  F32Sub = 0x93,
  F32Mul = 0x94,
  F32Div = 0x95,
  F32Min = 0x96,
  F32Max = 0x97,
  F32Copysign = 0x98,

  // Numeric (f64)
  F64Abs = 0x99,
  F64Neg = 0x9a,
  F64Ceil = 0x9b,
  F64Floor = 0x9c,
  F64Trunc = 0x9d,
  F64Nearest = 0x9e,
  F64Sqrt = 0x9f,
  F64Add = 0xa0,
  F64Sub = 0xa1,
  F64Mul = 0xa2,
  F64Div = 0xa3,
  F64Min = 0xa4,
  F64Max = 0xa5,
  F64Copysign = 0xa6,

  // Conversions
  I32WrapI64 = 0xa7,
  I32TruncF32S = 0xa8,
  I32TruncF32U = 0xa9,
  I32TruncF64S = 0xaa,
  I32TruncF64U = 0xab,
  I64ExtendI32S = 0xac,
  I64ExtendI32U = 0xad,
  I64TruncF32S = 0xae,
  I64TruncF32U = 0xaf,
  I64TruncF64S = 0xb0,
  I64TruncF64U = 0xb1,
  F32ConvertI32S = 0xb2,
  F32ConvertI32U = 0xb3,
  F32ConvertI64S = 0xb4,
  F32ConvertI64U = 0xb5,
  F32DemoteF64 = 0xb6,
  F64ConvertI32S = 0xb7,
  F64ConvertI32U = 0xb8,
  F64ConvertI64S = 0xb9,
  F64ConvertI64U = 0xba,
  F64PromoteF32 = 0xbb,
  I32ReinterpretF32 = 0xbc,
  I64ReinterpretF64 = 0xbd,
  F32ReinterpretI32 = 0xbe,
  F64ReinterpretI64 = 0xbf,

  // Sign extension
  I32Extend8S = 0xc0,
  I32Extend16S = 0xc1,
  I64Extend8S = 0xc2,
  I64Extend16S = 0xc3,
  I64Extend32S = 0xc4,
}
