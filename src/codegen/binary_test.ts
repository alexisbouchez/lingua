/**
 * Binary Encoding Tests
 */

import { assertEquals } from "jsr:@std/assert";
import { describe, it } from "jsr:@std/testing/bdd";
import { BinaryWriter, WASM_MAGIC, WASM_VERSION } from "./binary.ts";

describe("BinaryWriter", () => {
  describe("writeBytes", () => {
    it("writes raw bytes", () => {
      const writer = new BinaryWriter();
      writer.writeBytes([0x00, 0x61, 0x73, 0x6d]);
      assertEquals(writer.toBytes(), WASM_MAGIC);
    });
  });

  describe("writeByte", () => {
    it("writes a single byte", () => {
      const writer = new BinaryWriter();
      writer.writeByte(0x42);
      assertEquals(writer.toBytes(), new Uint8Array([0x42]));
    });

    it("truncates to byte", () => {
      const writer = new BinaryWriter();
      writer.writeByte(0x1ff);
      assertEquals(writer.toBytes(), new Uint8Array([0xff]));
    });
  });

  describe("writeU32 (unsigned LEB128)", () => {
    it("encodes 0", () => {
      const writer = new BinaryWriter();
      writer.writeU32(0);
      assertEquals(writer.toBytes(), new Uint8Array([0x00]));
    });

    it("encodes 1", () => {
      const writer = new BinaryWriter();
      writer.writeU32(1);
      assertEquals(writer.toBytes(), new Uint8Array([0x01]));
    });

    it("encodes 127 (max single byte)", () => {
      const writer = new BinaryWriter();
      writer.writeU32(127);
      assertEquals(writer.toBytes(), new Uint8Array([0x7f]));
    });

    it("encodes 128 (two bytes)", () => {
      const writer = new BinaryWriter();
      writer.writeU32(128);
      assertEquals(writer.toBytes(), new Uint8Array([0x80, 0x01]));
    });

    it("encodes 624485", () => {
      const writer = new BinaryWriter();
      writer.writeU32(624485);
      assertEquals(writer.toBytes(), new Uint8Array([0xe5, 0x8e, 0x26]));
    });
  });

  describe("writeI32 (signed LEB128)", () => {
    it("encodes 0", () => {
      const writer = new BinaryWriter();
      writer.writeI32(0);
      assertEquals(writer.toBytes(), new Uint8Array([0x00]));
    });

    it("encodes 1", () => {
      const writer = new BinaryWriter();
      writer.writeI32(1);
      assertEquals(writer.toBytes(), new Uint8Array([0x01]));
    });

    it("encodes -1", () => {
      const writer = new BinaryWriter();
      writer.writeI32(-1);
      assertEquals(writer.toBytes(), new Uint8Array([0x7f]));
    });

    it("encodes 63 (max positive single byte)", () => {
      const writer = new BinaryWriter();
      writer.writeI32(63);
      assertEquals(writer.toBytes(), new Uint8Array([0x3f]));
    });

    it("encodes 64 (two bytes)", () => {
      const writer = new BinaryWriter();
      writer.writeI32(64);
      assertEquals(writer.toBytes(), new Uint8Array([0xc0, 0x00]));
    });

    it("encodes -64 (single byte)", () => {
      const writer = new BinaryWriter();
      writer.writeI32(-64);
      assertEquals(writer.toBytes(), new Uint8Array([0x40]));
    });

    it("encodes -65 (two bytes)", () => {
      const writer = new BinaryWriter();
      writer.writeI32(-65);
      assertEquals(writer.toBytes(), new Uint8Array([0xbf, 0x7f]));
    });

    it("encodes -123456", () => {
      const writer = new BinaryWriter();
      writer.writeI32(-123456);
      assertEquals(writer.toBytes(), new Uint8Array([0xc0, 0xbb, 0x78]));
    });
  });

  describe("writeI64 (signed LEB128 64-bit)", () => {
    it("encodes 0n", () => {
      const writer = new BinaryWriter();
      writer.writeI64(0n);
      assertEquals(writer.toBytes(), new Uint8Array([0x00]));
    });

    it("encodes large positive", () => {
      const writer = new BinaryWriter();
      writer.writeI64(9223372036854775807n);
      // Max i64 - requires 10 bytes in signed LEB128
      assertEquals(writer.toBytes(), new Uint8Array([0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00]));
    });

    it("encodes -1n", () => {
      const writer = new BinaryWriter();
      writer.writeI64(-1n);
      assertEquals(writer.toBytes(), new Uint8Array([0x7f]));
    });
  });

  describe("writeF32", () => {
    it("encodes 0.0", () => {
      const writer = new BinaryWriter();
      writer.writeF32(0.0);
      assertEquals(writer.toBytes(), new Uint8Array([0x00, 0x00, 0x00, 0x00]));
    });

    it("encodes 1.0", () => {
      const writer = new BinaryWriter();
      writer.writeF32(1.0);
      assertEquals(writer.toBytes(), new Uint8Array([0x00, 0x00, 0x80, 0x3f]));
    });
  });

  describe("writeF64", () => {
    it("encodes 0.0", () => {
      const writer = new BinaryWriter();
      writer.writeF64(0.0);
      assertEquals(writer.toBytes(), new Uint8Array([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]));
    });

    it("encodes 1.0", () => {
      const writer = new BinaryWriter();
      writer.writeF64(1.0);
      assertEquals(writer.toBytes(), new Uint8Array([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f]));
    });
  });

  describe("writeName", () => {
    it("writes empty string", () => {
      const writer = new BinaryWriter();
      writer.writeName("");
      assertEquals(writer.toBytes(), new Uint8Array([0x00]));
    });

    it("writes ASCII string", () => {
      const writer = new BinaryWriter();
      writer.writeName("main");
      assertEquals(writer.toBytes(), new Uint8Array([0x04, 0x6d, 0x61, 0x69, 0x6e]));
    });
  });

  describe("writeVec", () => {
    it("writes empty vector", () => {
      const writer = new BinaryWriter();
      writer.writeVec([], () => {});
      assertEquals(writer.toBytes(), new Uint8Array([0x00]));
    });

    it("writes vector of bytes", () => {
      const writer = new BinaryWriter();
      writer.writeVec([1, 2, 3], (n) => writer.writeByte(n));
      assertEquals(writer.toBytes(), new Uint8Array([0x03, 0x01, 0x02, 0x03]));
    });
  });
});

describe("WASM Constants", () => {
  it("has correct magic number", () => {
    assertEquals(WASM_MAGIC, new Uint8Array([0x00, 0x61, 0x73, 0x6d]));
  });

  it("has correct version", () => {
    assertEquals(WASM_VERSION, new Uint8Array([0x01, 0x00, 0x00, 0x00]));
  });
});
