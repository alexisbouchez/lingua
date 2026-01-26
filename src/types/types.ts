/**
 * Lingua Type System - Type Representation
 *
 * Internal types used during type checking and inference.
 * These are separate from the AST Type nodes - AST represents syntax,
 * these represent semantics.
 */

// =============================================================================
// TYPE DEFINITIONS
// =============================================================================

export type Type =
  | PrimitiveType
  | FunctionType
  | TupleType
  | ArrayType
  | StructType
  | EnumType
  | GenericType
  | TypeVariable
  | NeverType
  | UnknownType;

// Primitive types that map directly to WASM
export interface PrimitiveType {
  kind: "Primitive";
  name: PrimitiveName;
}

export type PrimitiveName =
  | "i8" | "i16" | "i32" | "i64"
  | "u8" | "u16" | "u32" | "u64"
  | "f32" | "f64"
  | "bool"
  | "char"
  | "str"
  | "unit"; // () type

// Function types
export interface FunctionType {
  kind: "Function";
  params: Type[];
  ret: Type;
}

// Tuple types
export interface TupleType {
  kind: "Tuple";
  elements: Type[];
}

// Array types (dynamic length)
export interface ArrayType {
  kind: "Array";
  element: Type;
}

// Struct types (nominal)
export interface StructType {
  kind: "Struct";
  name: string;
  fields: Map<string, Type>;
  typeParams?: TypeVariable[];
}

// Enum types (nominal)
export interface EnumType {
  kind: "Enum";
  name: string;
  variants: Map<string, Type[] | null>; // null for unit variants
  typeParams?: TypeVariable[];
}

// Generic/Parameterized types (e.g., Option<T>, Vec<T>)
export interface GenericType {
  kind: "Generic";
  base: Type;
  args: Type[];
}

// Type variables for inference (e.g., ?T0, ?T1)
export interface TypeVariable {
  kind: "TypeVar";
  id: number;
  name?: string; // For named type params like T, U
}

// Never type (for functions that don't return, like panic)
export interface NeverType {
  kind: "Never";
}

// Unknown type (for error recovery)
export interface UnknownType {
  kind: "Unknown";
}

// =============================================================================
// TYPE CONSTRUCTORS
// =============================================================================

let typeVarCounter = 0;

export function resetTypeVarCounter(): void {
  typeVarCounter = 0;
}

export function freshTypeVar(name?: string): TypeVariable {
  return { kind: "TypeVar", id: typeVarCounter++, name };
}

export function primitive(name: PrimitiveName): PrimitiveType {
  return { kind: "Primitive", name };
}

export function functionType(params: Type[], ret: Type): FunctionType {
  return { kind: "Function", params, ret };
}

export function tupleType(elements: Type[]): TupleType {
  return { kind: "Tuple", elements };
}

export function arrayType(element: Type): ArrayType {
  return { kind: "Array", element };
}

export function structType(
  name: string,
  fields: Map<string, Type>,
  typeParams?: TypeVariable[],
): StructType {
  return { kind: "Struct", name, fields, typeParams };
}

export function enumType(
  name: string,
  variants: Map<string, Type[] | null>,
  typeParams?: TypeVariable[],
): EnumType {
  return { kind: "Enum", name, variants, typeParams };
}

export function genericType(base: Type, args: Type[]): GenericType {
  return { kind: "Generic", base, args };
}

export const UNIT: PrimitiveType = primitive("unit");
export const BOOL: PrimitiveType = primitive("bool");
export const I32: PrimitiveType = primitive("i32");
export const I64: PrimitiveType = primitive("i64");
export const F32: PrimitiveType = primitive("f32");
export const F64: PrimitiveType = primitive("f64");
export const STR: PrimitiveType = primitive("str");
export const CHAR: PrimitiveType = primitive("char");
export const NEVER: NeverType = { kind: "Never" };
export const UNKNOWN: UnknownType = { kind: "Unknown" };

// =============================================================================
// TYPE UTILITIES
// =============================================================================

export function typeToString(t: Type): string {
  switch (t.kind) {
    case "Primitive":
      return t.name === "unit" ? "()" : t.name;
    case "Function": {
      const params = t.params.map(typeToString).join(", ");
      const ret = typeToString(t.ret);
      return `fn(${params}) -> ${ret}`;
    }
    case "Tuple": {
      const elements = t.elements.map(typeToString).join(", ");
      return `(${elements})`;
    }
    case "Array":
      return `[${typeToString(t.element)}]`;
    case "Struct":
      return t.name;
    case "Enum":
      return t.name;
    case "Generic": {
      const base = typeToString(t.base);
      const args = t.args.map(typeToString).join(", ");
      return `${base}<${args}>`;
    }
    case "TypeVar":
      return t.name ?? `?T${t.id}`;
    case "Never":
      return "!";
    case "Unknown":
      return "?";
  }
}

export function typesEqual(a: Type, b: Type): boolean {
  if (a.kind !== b.kind) return false;

  switch (a.kind) {
    case "Primitive":
      return a.name === (b as PrimitiveType).name;
    case "Function": {
      const bf = b as FunctionType;
      if (a.params.length !== bf.params.length) return false;
      for (let i = 0; i < a.params.length; i++) {
        if (!typesEqual(a.params[i], bf.params[i])) return false;
      }
      return typesEqual(a.ret, bf.ret);
    }
    case "Tuple": {
      const bt = b as TupleType;
      if (a.elements.length !== bt.elements.length) return false;
      for (let i = 0; i < a.elements.length; i++) {
        if (!typesEqual(a.elements[i], bt.elements[i])) return false;
      }
      return true;
    }
    case "Array":
      return typesEqual(a.element, (b as ArrayType).element);
    case "Struct":
      return a.name === (b as StructType).name;
    case "Enum":
      return a.name === (b as EnumType).name;
    case "Generic": {
      const bg = b as GenericType;
      if (!typesEqual(a.base, bg.base)) return false;
      if (a.args.length !== bg.args.length) return false;
      for (let i = 0; i < a.args.length; i++) {
        if (!typesEqual(a.args[i], bg.args[i])) return false;
      }
      return true;
    }
    case "TypeVar":
      return a.id === (b as TypeVariable).id;
    case "Never":
    case "Unknown":
      return true;
  }
}

// Check if a type contains a type variable
export function containsTypeVar(t: Type, varId: number): boolean {
  switch (t.kind) {
    case "Primitive":
    case "Never":
    case "Unknown":
      return false;
    case "TypeVar":
      return t.id === varId;
    case "Function":
      return t.params.some((p) => containsTypeVar(p, varId)) ||
        containsTypeVar(t.ret, varId);
    case "Tuple":
      return t.elements.some((e) => containsTypeVar(e, varId));
    case "Array":
      return containsTypeVar(t.element, varId);
    case "Struct":
      for (const fieldType of t.fields.values()) {
        if (containsTypeVar(fieldType, varId)) return true;
      }
      return false;
    case "Enum":
      for (const variantTypes of t.variants.values()) {
        if (variantTypes) {
          for (const vt of variantTypes) {
            if (containsTypeVar(vt, varId)) return true;
          }
        }
      }
      return false;
    case "Generic":
      return containsTypeVar(t.base, varId) ||
        t.args.some((a) => containsTypeVar(a, varId));
  }
}

// Substitute a type variable with a concrete type
export function substitute(t: Type, varId: number, replacement: Type): Type {
  switch (t.kind) {
    case "Primitive":
    case "Never":
    case "Unknown":
      return t;
    case "TypeVar":
      return t.id === varId ? replacement : t;
    case "Function":
      return functionType(
        t.params.map((p) => substitute(p, varId, replacement)),
        substitute(t.ret, varId, replacement),
      );
    case "Tuple":
      return tupleType(t.elements.map((e) => substitute(e, varId, replacement)));
    case "Array":
      return arrayType(substitute(t.element, varId, replacement));
    case "Struct": {
      const newFields = new Map<string, Type>();
      for (const [name, fieldType] of t.fields) {
        newFields.set(name, substitute(fieldType, varId, replacement));
      }
      return structType(t.name, newFields, t.typeParams);
    }
    case "Enum": {
      const newVariants = new Map<string, Type[] | null>();
      for (const [name, variantTypes] of t.variants) {
        if (variantTypes) {
          newVariants.set(
            name,
            variantTypes.map((vt) => substitute(vt, varId, replacement)),
          );
        } else {
          newVariants.set(name, null);
        }
      }
      return enumType(t.name, newVariants, t.typeParams);
    }
    case "Generic":
      return genericType(
        substitute(t.base, varId, replacement),
        t.args.map((a) => substitute(a, varId, replacement)),
      );
  }
}

// Check if type is numeric
export function isNumeric(t: Type): boolean {
  if (t.kind !== "Primitive") return false;
  return ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64"]
    .includes(t.name);
}

// Check if type is integer
export function isInteger(t: Type): boolean {
  if (t.kind !== "Primitive") return false;
  return ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64"].includes(t.name);
}

// Check if type is float
export function isFloat(t: Type): boolean {
  if (t.kind !== "Primitive") return false;
  return ["f32", "f64"].includes(t.name);
}

// Check if type is signed integer
export function isSigned(t: Type): boolean {
  if (t.kind !== "Primitive") return false;
  return ["i8", "i16", "i32", "i64"].includes(t.name);
}
