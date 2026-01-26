/**
 * WASM Module Builder Tests
 */

import { assertEquals, assertArrayIncludes } from "jsr:@std/assert";
import { describe, it } from "jsr:@std/testing/bdd";
import { WasmModule, CodeBuilder, ValType } from "./wasm.ts";
import { WASM_MAGIC, WASM_VERSION } from "./binary.ts";

describe("WasmModule", () => {
  describe("emit", () => {
    it("emits magic and version", () => {
      const module = new WasmModule();
      const bytes = module.emit();

      // Check magic
      assertEquals(bytes.slice(0, 4), WASM_MAGIC);
      // Check version
      assertEquals(bytes.slice(4, 8), WASM_VERSION);
    });
  });

  describe("types", () => {
    it("adds function types", () => {
      const module = new WasmModule();
      const idx = module.addType([ValType.I32, ValType.I32], [ValType.I32]);
      assertEquals(idx, 0);

      const idx2 = module.addType([], []);
      assertEquals(idx2, 1);
    });

    it("deduplicates types", () => {
      const module = new WasmModule();
      const idx1 = module.getOrAddType([ValType.I32], [ValType.I32]);
      const idx2 = module.getOrAddType([ValType.I32], [ValType.I32]);
      assertEquals(idx1, idx2);
    });
  });

  describe("functions", () => {
    it("adds functions", () => {
      const module = new WasmModule();
      const typeIdx = module.addType([ValType.I32], [ValType.I32]);
      const funcIdx = module.addFunction(typeIdx);
      assertEquals(funcIdx, 0);
    });

    it("accounts for imports in function indices", () => {
      const module = new WasmModule();
      const typeIdx = module.addType([], []);

      // Add import first
      const importIdx = module.addFuncImport("env", "print", typeIdx);
      assertEquals(importIdx, 0);

      // Regular function should be index 1
      const funcIdx = module.addFunction(typeIdx);
      assertEquals(funcIdx, 1);
    });
  });

  describe("exports", () => {
    it("exports functions", () => {
      const module = new WasmModule();
      const typeIdx = module.addType([], []);
      const funcIdx = module.addFunction(typeIdx);
      module.addCode(typeIdx, [], []);
      module.exportFunc("main", funcIdx);

      const bytes = module.emit();
      // Should contain "main" in export section
      const mainBytes = new TextEncoder().encode("main");
      assertArrayIncludes([...bytes], [...mainBytes]);
    });
  });

  describe("memory", () => {
    it("adds memory without max", () => {
      const module = new WasmModule();
      const memIdx = module.addMemory(1);
      assertEquals(memIdx, 0);
    });

    it("adds memory with max", () => {
      const module = new WasmModule();
      const memIdx = module.addMemory(1, 10);
      assertEquals(memIdx, 0);
    });
  });

  describe("full module", () => {
    it("creates valid module with function", () => {
      const module = new WasmModule();

      // Add type: () -> i32
      const typeIdx = module.addType([], [ValType.I32]);

      // Add function
      const funcIdx = module.addFunction(typeIdx);

      // Add code: return 42
      module.addCode(typeIdx, [], [
        0x41, 0x2a,  // i32.const 42
      ]);

      // Export as "answer"
      module.exportFunc("answer", funcIdx);

      const bytes = module.emit();

      // Verify it's a valid WASM module
      assertEquals(bytes.slice(0, 4), WASM_MAGIC);
      assertEquals(bytes.slice(4, 8), WASM_VERSION);
    });

    it("creates module with add function", () => {
      const module = new WasmModule();

      // Add type: (i32, i32) -> i32
      const typeIdx = module.addType([ValType.I32, ValType.I32], [ValType.I32]);

      // Add function
      const funcIdx = module.addFunction(typeIdx);

      // Add code: return a + b
      module.addCode(typeIdx, [], [
        0x20, 0x00,  // local.get 0
        0x20, 0x01,  // local.get 1
        0x6a,        // i32.add
      ]);

      // Export
      module.exportFunc("add", funcIdx);

      const bytes = module.emit();
      assertEquals(bytes.slice(0, 4), WASM_MAGIC);
    });
  });
});

describe("CodeBuilder", () => {
  describe("constants", () => {
    it("emits i32.const", () => {
      const builder = new CodeBuilder();
      builder.i32Const(42);
      assertEquals(builder.getCode(), [0x41, 0x2a]);
    });

    it("emits i32.const negative", () => {
      const builder = new CodeBuilder();
      builder.i32Const(-1);
      assertEquals(builder.getCode(), [0x41, 0x7f]);
    });

    it("emits f64.const", () => {
      const builder = new CodeBuilder();
      builder.f64Const(1.0);
      const code = builder.getCode();
      assertEquals(code[0], 0x44);  // f64.const opcode
      assertEquals(code.length, 9);  // opcode + 8 bytes
    });
  });

  describe("locals", () => {
    it("adds locals", () => {
      const builder = new CodeBuilder();
      const idx0 = builder.addLocal(ValType.I32);
      const idx1 = builder.addLocal(ValType.I32);
      const idx2 = builder.addLocal(ValType.I64);

      assertEquals(idx0, 0);
      assertEquals(idx1, 1);
      assertEquals(idx2, 2);
    });

    it("groups locals by type", () => {
      const builder = new CodeBuilder();
      builder.addLocal(ValType.I32);
      builder.addLocal(ValType.I32);
      builder.addLocal(ValType.I64);

      const locals = builder.getLocals();
      assertEquals(locals.length, 2);
      assertEquals(locals[0], { count: 2, type: ValType.I32 });
      assertEquals(locals[1], { count: 1, type: ValType.I64 });
    });
  });

  describe("local operations", () => {
    it("emits local.get", () => {
      const builder = new CodeBuilder();
      builder.localGet(0);
      assertEquals(builder.getCode(), [0x20, 0x00]);
    });

    it("emits local.set", () => {
      const builder = new CodeBuilder();
      builder.localSet(1);
      assertEquals(builder.getCode(), [0x21, 0x01]);
    });
  });

  describe("arithmetic", () => {
    it("emits i32.add", () => {
      const builder = new CodeBuilder();
      builder.i32Add();
      assertEquals(builder.getCode(), [0x6a]);
    });

    it("emits i32.sub", () => {
      const builder = new CodeBuilder();
      builder.i32Sub();
      assertEquals(builder.getCode(), [0x6b]);
    });

    it("emits i32.mul", () => {
      const builder = new CodeBuilder();
      builder.i32Mul();
      assertEquals(builder.getCode(), [0x6c]);
    });
  });

  describe("comparison", () => {
    it("emits i32.eq", () => {
      const builder = new CodeBuilder();
      builder.i32Eq();
      assertEquals(builder.getCode(), [0x46]);
    });

    it("emits i32.lt_s", () => {
      const builder = new CodeBuilder();
      builder.i32LtS();
      assertEquals(builder.getCode(), [0x48]);
    });
  });

  describe("control flow", () => {
    it("emits block/end", () => {
      const builder = new CodeBuilder();
      builder.block();
      builder.end();
      assertEquals(builder.getCode(), [0x02, 0x40, 0x0b]);
    });

    it("emits if/else/end", () => {
      const builder = new CodeBuilder();
      builder.if_();
      builder.i32Const(1);
      builder.else_();
      builder.i32Const(0);
      builder.end();

      const code = builder.getCode();
      assertEquals(code[0], 0x04);  // if
      assertEquals(code.includes(0x05), true);  // else
      assertEquals(code[code.length - 1], 0x0b);  // end
    });

    it("emits br", () => {
      const builder = new CodeBuilder();
      builder.br(0);
      assertEquals(builder.getCode(), [0x0c, 0x00]);
    });

    it("emits br_if", () => {
      const builder = new CodeBuilder();
      builder.brIf(1);
      assertEquals(builder.getCode(), [0x0d, 0x01]);
    });

    it("emits call", () => {
      const builder = new CodeBuilder();
      builder.call(5);
      assertEquals(builder.getCode(), [0x10, 0x05]);
    });

    it("emits return", () => {
      const builder = new CodeBuilder();
      builder.return_();
      assertEquals(builder.getCode(), [0x0f]);
    });
  });
});
