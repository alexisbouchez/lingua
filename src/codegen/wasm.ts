/**
 * WASM Module Builder
 *
 * Provides a high-level API for building WASM modules.
 */

import {
  BinaryWriter,
  WASM_MAGIC,
  WASM_VERSION,
  SectionId,
  ValType,
  ExportKind,
  LimitsFlags,
  FUNC_TYPE,
  BLOCK_TYPE_VOID,
  Op,
} from "./binary.ts";

// Re-export for convenience
export { ValType, BLOCK_TYPE_VOID };

// =============================================================================
// WASM TYPE DEFINITIONS
// =============================================================================

export interface FuncType {
  params: ValType[];
  results: ValType[];
}

export interface Import {
  module: string;
  name: string;
  kind: ImportKind;
}

export type ImportKind =
  | { type: "func"; typeIdx: number }
  | { type: "table"; tableType: TableType }
  | { type: "memory"; limits: Limits }
  | { type: "global"; globalType: GlobalType };

export interface TableType {
  elemType: ValType;
  limits: Limits;
}

export interface Limits {
  min: number;
  max?: number;
}

export interface GlobalType {
  valType: ValType;
  mutable: boolean;
}

export interface Global {
  type: GlobalType;
  init: number[];  // init expression bytes
}

export interface Export {
  name: string;
  kind: ExportKind;
  index: number;
}

export interface Local {
  count: number;
  type: ValType;
}

export interface WasmFunc {
  typeIdx: number;
  locals: Local[];
  body: number[];  // instruction bytes
}

// =============================================================================
// WASM MODULE
// =============================================================================

export class WasmModule {
  private types: FuncType[] = [];
  private imports: Import[] = [];
  private functions: number[] = [];  // type indices
  private tables: TableType[] = [];
  private memories: Limits[] = [];
  private globals: Global[] = [];
  private exports: Export[] = [];
  private start: number | null = null;
  private codes: WasmFunc[] = [];

  // Track counts for proper indexing
  private importFuncCount = 0;
  private importGlobalCount = 0;

  // ---------------------------------------------------------------------------
  // TYPE SECTION
  // ---------------------------------------------------------------------------

  addType(params: ValType[], results: ValType[]): number {
    const idx = this.types.length;
    this.types.push({ params, results });
    return idx;
  }

  getOrAddType(params: ValType[], results: ValType[]): number {
    // Check if type already exists
    for (let i = 0; i < this.types.length; i++) {
      const t = this.types[i];
      if (arraysEqual(t.params, params) && arraysEqual(t.results, results)) {
        return i;
      }
    }
    return this.addType(params, results);
  }

  // ---------------------------------------------------------------------------
  // IMPORT SECTION
  // ---------------------------------------------------------------------------

  addFuncImport(module: string, name: string, typeIdx: number): number {
    const funcIdx = this.importFuncCount++;
    this.imports.push({
      module,
      name,
      kind: { type: "func", typeIdx },
    });
    return funcIdx;
  }

  addMemoryImport(module: string, name: string, min: number, max?: number): void {
    this.imports.push({
      module,
      name,
      kind: { type: "memory", limits: { min, max } },
    });
  }

  addGlobalImport(module: string, name: string, valType: ValType, mutable: boolean): number {
    const globalIdx = this.importGlobalCount++;
    this.imports.push({
      module,
      name,
      kind: { type: "global", globalType: { valType, mutable } },
    });
    return globalIdx;
  }

  // ---------------------------------------------------------------------------
  // FUNCTION SECTION
  // ---------------------------------------------------------------------------

  addFunction(typeIdx: number): number {
    const funcIdx = this.importFuncCount + this.functions.length;
    this.functions.push(typeIdx);
    return funcIdx;
  }

  // ---------------------------------------------------------------------------
  // MEMORY SECTION
  // ---------------------------------------------------------------------------

  addMemory(min: number, max?: number): number {
    const idx = this.memories.length;
    this.memories.push({ min, max });
    return idx;
  }

  // ---------------------------------------------------------------------------
  // GLOBAL SECTION
  // ---------------------------------------------------------------------------

  addGlobal(valType: ValType, mutable: boolean, init: number[]): number {
    const idx = this.importGlobalCount + this.globals.length;
    this.globals.push({
      type: { valType, mutable },
      init,
    });
    return idx;
  }

  // ---------------------------------------------------------------------------
  // EXPORT SECTION
  // ---------------------------------------------------------------------------

  exportFunc(name: string, funcIdx: number): void {
    this.exports.push({ name, kind: ExportKind.Func, index: funcIdx });
  }

  exportMemory(name: string, memIdx: number): void {
    this.exports.push({ name, kind: ExportKind.Memory, index: memIdx });
  }

  exportGlobal(name: string, globalIdx: number): void {
    this.exports.push({ name, kind: ExportKind.Global, index: globalIdx });
  }

  // ---------------------------------------------------------------------------
  // START SECTION
  // ---------------------------------------------------------------------------

  setStart(funcIdx: number): void {
    this.start = funcIdx;
  }

  // ---------------------------------------------------------------------------
  // CODE SECTION
  // ---------------------------------------------------------------------------

  addCode(typeIdx: number, locals: Local[], body: number[]): void {
    this.codes.push({ typeIdx, locals, body });
  }

  // ---------------------------------------------------------------------------
  // EMIT MODULE
  // ---------------------------------------------------------------------------

  emit(): Uint8Array {
    const writer = new BinaryWriter();

    // Magic and version
    writer.writeBytes(WASM_MAGIC);
    writer.writeBytes(WASM_VERSION);

    // Type section
    if (this.types.length > 0) {
      this.emitTypeSection(writer);
    }

    // Import section
    if (this.imports.length > 0) {
      this.emitImportSection(writer);
    }

    // Function section
    if (this.functions.length > 0) {
      this.emitFunctionSection(writer);
    }

    // Memory section
    if (this.memories.length > 0) {
      this.emitMemorySection(writer);
    }

    // Global section
    if (this.globals.length > 0) {
      this.emitGlobalSection(writer);
    }

    // Export section
    if (this.exports.length > 0) {
      this.emitExportSection(writer);
    }

    // Start section
    if (this.start !== null) {
      this.emitStartSection(writer);
    }

    // Code section
    if (this.codes.length > 0) {
      this.emitCodeSection(writer);
    }

    return writer.toBytes();
  }

  private emitSection(writer: BinaryWriter, sectionId: SectionId, emitContent: (w: BinaryWriter) => void): void {
    const content = new BinaryWriter();
    emitContent(content);
    const bytes = content.toBytes();

    writer.writeByte(sectionId);
    writer.writeU32(bytes.length);
    writer.writeBytes(bytes);
  }

  private emitTypeSection(writer: BinaryWriter): void {
    this.emitSection(writer, SectionId.Type, (w) => {
      w.writeU32(this.types.length);
      for (const type of this.types) {
        w.writeByte(FUNC_TYPE);
        w.writeVec(type.params, (p) => w.writeByte(p));
        w.writeVec(type.results, (r) => w.writeByte(r));
      }
    });
  }

  private emitImportSection(writer: BinaryWriter): void {
    this.emitSection(writer, SectionId.Import, (w) => {
      w.writeU32(this.imports.length);
      for (const imp of this.imports) {
        w.writeName(imp.module);
        w.writeName(imp.name);
        switch (imp.kind.type) {
          case "func":
            w.writeByte(0x00);
            w.writeU32(imp.kind.typeIdx);
            break;
          case "table":
            w.writeByte(0x01);
            w.writeByte(imp.kind.tableType.elemType);
            this.emitLimits(w, imp.kind.tableType.limits);
            break;
          case "memory":
            w.writeByte(0x02);
            this.emitLimits(w, imp.kind.limits);
            break;
          case "global":
            w.writeByte(0x03);
            w.writeByte(imp.kind.globalType.valType);
            w.writeByte(imp.kind.globalType.mutable ? 0x01 : 0x00);
            break;
        }
      }
    });
  }

  private emitFunctionSection(writer: BinaryWriter): void {
    this.emitSection(writer, SectionId.Function, (w) => {
      w.writeVec(this.functions, (typeIdx) => w.writeU32(typeIdx));
    });
  }

  private emitMemorySection(writer: BinaryWriter): void {
    this.emitSection(writer, SectionId.Memory, (w) => {
      w.writeU32(this.memories.length);
      for (const mem of this.memories) {
        this.emitLimits(w, mem);
      }
    });
  }

  private emitGlobalSection(writer: BinaryWriter): void {
    this.emitSection(writer, SectionId.Global, (w) => {
      w.writeU32(this.globals.length);
      for (const g of this.globals) {
        w.writeByte(g.type.valType);
        w.writeByte(g.type.mutable ? 0x01 : 0x00);
        w.writeBytes(g.init);
        w.writeByte(Op.End);
      }
    });
  }

  private emitExportSection(writer: BinaryWriter): void {
    this.emitSection(writer, SectionId.Export, (w) => {
      w.writeU32(this.exports.length);
      for (const exp of this.exports) {
        w.writeName(exp.name);
        w.writeByte(exp.kind);
        w.writeU32(exp.index);
      }
    });
  }

  private emitStartSection(writer: BinaryWriter): void {
    this.emitSection(writer, SectionId.Start, (w) => {
      w.writeU32(this.start!);
    });
  }

  private emitCodeSection(writer: BinaryWriter): void {
    this.emitSection(writer, SectionId.Code, (w) => {
      w.writeU32(this.codes.length);
      for (const code of this.codes) {
        // Emit function body
        const bodyWriter = new BinaryWriter();

        // Locals
        bodyWriter.writeU32(code.locals.length);
        for (const local of code.locals) {
          bodyWriter.writeU32(local.count);
          bodyWriter.writeByte(local.type);
        }

        // Instructions
        bodyWriter.writeBytes(code.body);
        bodyWriter.writeByte(Op.End);

        const bodyBytes = bodyWriter.toBytes();
        w.writeU32(bodyBytes.length);
        w.writeBytes(bodyBytes);
      }
    });
  }

  private emitLimits(w: BinaryWriter, limits: Limits): void {
    if (limits.max !== undefined) {
      w.writeByte(LimitsFlags.HasMax);
      w.writeU32(limits.min);
      w.writeU32(limits.max);
    } else {
      w.writeByte(LimitsFlags.NoMax);
      w.writeU32(limits.min);
    }
  }
}

// =============================================================================
// CODE BUILDER (for building function bodies)
// =============================================================================

export class CodeBuilder {
  private code: number[] = [];
  private localCount = 0;
  private localTypes: ValType[] = [];

  // Get the generated code bytes
  getCode(): number[] {
    return this.code;
  }

  // Get locals grouped by type
  getLocals(): Local[] {
    const groups: Local[] = [];
    let currentType: ValType | null = null;
    let currentCount = 0;

    for (const type of this.localTypes) {
      if (type === currentType) {
        currentCount++;
      } else {
        if (currentType !== null) {
          groups.push({ count: currentCount, type: currentType });
        }
        currentType = type;
        currentCount = 1;
      }
    }

    if (currentType !== null) {
      groups.push({ count: currentCount, type: currentType });
    }

    return groups;
  }

  // Add a local variable, returns its index
  addLocal(type: ValType): number {
    const idx = this.localCount++;
    this.localTypes.push(type);
    return idx;
  }

  // ---------------------------------------------------------------------------
  // CONTROL FLOW
  // ---------------------------------------------------------------------------

  unreachable(): void {
    this.code.push(Op.Unreachable);
  }

  nop(): void {
    this.code.push(Op.Nop);
  }

  block(blockType: number = BLOCK_TYPE_VOID): void {
    this.code.push(Op.Block);
    this.code.push(blockType);
  }

  loop(blockType: number = BLOCK_TYPE_VOID): void {
    this.code.push(Op.Loop);
    this.code.push(blockType);
  }

  if_(blockType: number = BLOCK_TYPE_VOID): void {
    this.code.push(Op.If);
    this.code.push(blockType);
  }

  else_(): void {
    this.code.push(Op.Else);
  }

  end(): void {
    this.code.push(Op.End);
  }

  br(labelIdx: number): void {
    this.code.push(Op.Br);
    this.pushU32(labelIdx);
  }

  brIf(labelIdx: number): void {
    this.code.push(Op.BrIf);
    this.pushU32(labelIdx);
  }

  return_(): void {
    this.code.push(Op.Return);
  }

  call(funcIdx: number): void {
    this.code.push(Op.Call);
    this.pushU32(funcIdx);
  }

  // ---------------------------------------------------------------------------
  // PARAMETRIC
  // ---------------------------------------------------------------------------

  drop(): void {
    this.code.push(Op.Drop);
  }

  select(): void {
    this.code.push(Op.Select);
  }

  // ---------------------------------------------------------------------------
  // VARIABLES
  // ---------------------------------------------------------------------------

  localGet(idx: number): void {
    this.code.push(Op.LocalGet);
    this.pushU32(idx);
  }

  localSet(idx: number): void {
    this.code.push(Op.LocalSet);
    this.pushU32(idx);
  }

  localTee(idx: number): void {
    this.code.push(Op.LocalTee);
    this.pushU32(idx);
  }

  globalGet(idx: number): void {
    this.code.push(Op.GlobalGet);
    this.pushU32(idx);
  }

  globalSet(idx: number): void {
    this.code.push(Op.GlobalSet);
    this.pushU32(idx);
  }

  // ---------------------------------------------------------------------------
  // CONSTANTS
  // ---------------------------------------------------------------------------

  i32Const(value: number): void {
    this.code.push(Op.I32Const);
    this.pushI32(value);
  }

  i64Const(value: bigint): void {
    this.code.push(Op.I64Const);
    this.pushI64(value);
  }

  f32Const(value: number): void {
    this.code.push(Op.F32Const);
    this.pushF32(value);
  }

  f64Const(value: number): void {
    this.code.push(Op.F64Const);
    this.pushF64(value);
  }

  // ---------------------------------------------------------------------------
  // COMPARISON (i32)
  // ---------------------------------------------------------------------------

  i32Eqz(): void { this.code.push(Op.I32Eqz); }
  i32Eq(): void { this.code.push(Op.I32Eq); }
  i32Ne(): void { this.code.push(Op.I32Ne); }
  i32LtS(): void { this.code.push(Op.I32LtS); }
  i32LtU(): void { this.code.push(Op.I32LtU); }
  i32GtS(): void { this.code.push(Op.I32GtS); }
  i32GtU(): void { this.code.push(Op.I32GtU); }
  i32LeS(): void { this.code.push(Op.I32LeS); }
  i32LeU(): void { this.code.push(Op.I32LeU); }
  i32GeS(): void { this.code.push(Op.I32GeS); }
  i32GeU(): void { this.code.push(Op.I32GeU); }

  // ---------------------------------------------------------------------------
  // COMPARISON (i64)
  // ---------------------------------------------------------------------------

  i64Eqz(): void { this.code.push(Op.I64Eqz); }
  i64Eq(): void { this.code.push(Op.I64Eq); }
  i64Ne(): void { this.code.push(Op.I64Ne); }
  i64LtS(): void { this.code.push(Op.I64LtS); }
  i64LtU(): void { this.code.push(Op.I64LtU); }
  i64GtS(): void { this.code.push(Op.I64GtS); }
  i64GtU(): void { this.code.push(Op.I64GtU); }
  i64LeS(): void { this.code.push(Op.I64LeS); }
  i64LeU(): void { this.code.push(Op.I64LeU); }
  i64GeS(): void { this.code.push(Op.I64GeS); }
  i64GeU(): void { this.code.push(Op.I64GeU); }

  // ---------------------------------------------------------------------------
  // COMPARISON (f32)
  // ---------------------------------------------------------------------------

  f32Eq(): void { this.code.push(Op.F32Eq); }
  f32Ne(): void { this.code.push(Op.F32Ne); }
  f32Lt(): void { this.code.push(Op.F32Lt); }
  f32Gt(): void { this.code.push(Op.F32Gt); }
  f32Le(): void { this.code.push(Op.F32Le); }
  f32Ge(): void { this.code.push(Op.F32Ge); }

  // ---------------------------------------------------------------------------
  // COMPARISON (f64)
  // ---------------------------------------------------------------------------

  f64Eq(): void { this.code.push(Op.F64Eq); }
  f64Ne(): void { this.code.push(Op.F64Ne); }
  f64Lt(): void { this.code.push(Op.F64Lt); }
  f64Gt(): void { this.code.push(Op.F64Gt); }
  f64Le(): void { this.code.push(Op.F64Le); }
  f64Ge(): void { this.code.push(Op.F64Ge); }

  // ---------------------------------------------------------------------------
  // NUMERIC (i32)
  // ---------------------------------------------------------------------------

  i32Add(): void { this.code.push(Op.I32Add); }
  i32Sub(): void { this.code.push(Op.I32Sub); }
  i32Mul(): void { this.code.push(Op.I32Mul); }
  i32DivS(): void { this.code.push(Op.I32DivS); }
  i32DivU(): void { this.code.push(Op.I32DivU); }
  i32RemS(): void { this.code.push(Op.I32RemS); }
  i32RemU(): void { this.code.push(Op.I32RemU); }
  i32And(): void { this.code.push(Op.I32And); }
  i32Or(): void { this.code.push(Op.I32Or); }
  i32Xor(): void { this.code.push(Op.I32Xor); }
  i32Shl(): void { this.code.push(Op.I32Shl); }
  i32ShrS(): void { this.code.push(Op.I32ShrS); }
  i32ShrU(): void { this.code.push(Op.I32ShrU); }

  // ---------------------------------------------------------------------------
  // NUMERIC (i64)
  // ---------------------------------------------------------------------------

  i64Add(): void { this.code.push(Op.I64Add); }
  i64Sub(): void { this.code.push(Op.I64Sub); }
  i64Mul(): void { this.code.push(Op.I64Mul); }
  i64DivS(): void { this.code.push(Op.I64DivS); }
  i64DivU(): void { this.code.push(Op.I64DivU); }
  i64RemS(): void { this.code.push(Op.I64RemS); }
  i64RemU(): void { this.code.push(Op.I64RemU); }
  i64And(): void { this.code.push(Op.I64And); }
  i64Or(): void { this.code.push(Op.I64Or); }
  i64Xor(): void { this.code.push(Op.I64Xor); }
  i64Shl(): void { this.code.push(Op.I64Shl); }
  i64ShrS(): void { this.code.push(Op.I64ShrS); }
  i64ShrU(): void { this.code.push(Op.I64ShrU); }

  // ---------------------------------------------------------------------------
  // NUMERIC (f32)
  // ---------------------------------------------------------------------------

  f32Abs(): void { this.code.push(Op.F32Abs); }
  f32Neg(): void { this.code.push(Op.F32Neg); }
  f32Add(): void { this.code.push(Op.F32Add); }
  f32Sub(): void { this.code.push(Op.F32Sub); }
  f32Mul(): void { this.code.push(Op.F32Mul); }
  f32Div(): void { this.code.push(Op.F32Div); }

  // ---------------------------------------------------------------------------
  // NUMERIC (f64)
  // ---------------------------------------------------------------------------

  f64Abs(): void { this.code.push(Op.F64Abs); }
  f64Neg(): void { this.code.push(Op.F64Neg); }
  f64Add(): void { this.code.push(Op.F64Add); }
  f64Sub(): void { this.code.push(Op.F64Sub); }
  f64Mul(): void { this.code.push(Op.F64Mul); }
  f64Div(): void { this.code.push(Op.F64Div); }

  // ---------------------------------------------------------------------------
  // CONVERSIONS
  // ---------------------------------------------------------------------------

  i32WrapI64(): void { this.code.push(Op.I32WrapI64); }
  i64ExtendI32S(): void { this.code.push(Op.I64ExtendI32S); }
  i64ExtendI32U(): void { this.code.push(Op.I64ExtendI32U); }
  f32ConvertI32S(): void { this.code.push(Op.F32ConvertI32S); }
  f32ConvertI32U(): void { this.code.push(Op.F32ConvertI32U); }
  f32ConvertI64S(): void { this.code.push(Op.F32ConvertI64S); }
  f32ConvertI64U(): void { this.code.push(Op.F32ConvertI64U); }
  f32DemoteF64(): void { this.code.push(Op.F32DemoteF64); }
  f64ConvertI32S(): void { this.code.push(Op.F64ConvertI32S); }
  f64ConvertI32U(): void { this.code.push(Op.F64ConvertI32U); }
  f64ConvertI64S(): void { this.code.push(Op.F64ConvertI64S); }
  f64ConvertI64U(): void { this.code.push(Op.F64ConvertI64U); }
  f64PromoteF32(): void { this.code.push(Op.F64PromoteF32); }

  // ---------------------------------------------------------------------------
  // MEMORY
  // ---------------------------------------------------------------------------

  i32Load(align: number = 2, offset: number = 0): void {
    this.code.push(Op.I32Load);
    this.pushU32(align);
    this.pushU32(offset);
  }

  i32Load8S(align: number = 0, offset: number = 0): void {
    this.code.push(Op.I32Load8S);
    this.pushU32(align);
    this.pushU32(offset);
  }

  i32Load8U(align: number = 0, offset: number = 0): void {
    this.code.push(Op.I32Load8U);
    this.pushU32(align);
    this.pushU32(offset);
  }

  i32Load16S(align: number = 1, offset: number = 0): void {
    this.code.push(Op.I32Load16S);
    this.pushU32(align);
    this.pushU32(offset);
  }

  i32Load16U(align: number = 1, offset: number = 0): void {
    this.code.push(Op.I32Load16U);
    this.pushU32(align);
    this.pushU32(offset);
  }

  i32Store(align: number = 2, offset: number = 0): void {
    this.code.push(Op.I32Store);
    this.pushU32(align);
    this.pushU32(offset);
  }

  i32Store8(align: number = 0, offset: number = 0): void {
    this.code.push(Op.I32Store8);
    this.pushU32(align);
    this.pushU32(offset);
  }

  i32Store16(align: number = 1, offset: number = 0): void {
    this.code.push(Op.I32Store16);
    this.pushU32(align);
    this.pushU32(offset);
  }

  // ---------------------------------------------------------------------------
  // HELPERS
  // ---------------------------------------------------------------------------

  private pushU32(value: number): void {
    do {
      let byte = value & 0x7f;
      value >>>= 7;
      if (value !== 0) {
        byte |= 0x80;
      }
      this.code.push(byte);
    } while (value !== 0);
  }

  private pushI32(value: number): void {
    let more = true;
    while (more) {
      let byte = value & 0x7f;
      value >>= 7;
      const signBit = (byte & 0x40) !== 0;
      if ((value === 0 && !signBit) || (value === -1 && signBit)) {
        more = false;
      } else {
        byte |= 0x80;
      }
      this.code.push(byte);
    }
  }

  private pushI64(value: bigint): void {
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
      this.code.push(byte);
    }
  }

  private pushF32(value: number): void {
    const buf = new ArrayBuffer(4);
    new Float32Array(buf)[0] = value;
    const bytes = new Uint8Array(buf);
    for (let i = 0; i < 4; i++) {
      this.code.push(bytes[i]);
    }
  }

  private pushF64(value: number): void {
    const buf = new ArrayBuffer(8);
    new Float64Array(buf)[0] = value;
    const bytes = new Uint8Array(buf);
    for (let i = 0; i < 8; i++) {
      this.code.push(bytes[i]);
    }
  }
}

// =============================================================================
// HELPERS
// =============================================================================

function arraysEqual<T>(a: T[], b: T[]): boolean {
  if (a.length !== b.length) return false;
  for (let i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) return false;
  }
  return true;
}
