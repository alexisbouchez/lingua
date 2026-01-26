/**
 * Lingua Type System - Type Environment
 *
 * Manages type bindings and scopes during type checking.
 */

import type { Type, StructType, EnumType, FunctionType } from "./types.ts";

// =============================================================================
// SYMBOL TABLE
// =============================================================================

export interface Symbol {
  name: string;
  type: Type;
  mutable: boolean;
  kind: "variable" | "parameter" | "function";
}

export interface TypeDef {
  name: string;
  type: Type;
  kind: "struct" | "enum" | "alias";
}

// =============================================================================
// SCOPE
// =============================================================================

export class Scope {
  private symbols: Map<string, Symbol> = new Map();
  private types: Map<string, TypeDef> = new Map();
  readonly parent: Scope | null;

  constructor(parent: Scope | null = null) {
    this.parent = parent;
  }

  // Variables and functions

  define(name: string, type: Type, mutable: boolean, kind: Symbol["kind"]): void {
    this.symbols.set(name, { name, type, mutable, kind });
  }

  lookup(name: string): Symbol | undefined {
    const symbol = this.symbols.get(name);
    if (symbol) return symbol;
    return this.parent?.lookup(name);
  }

  lookupLocal(name: string): Symbol | undefined {
    return this.symbols.get(name);
  }

  isDefined(name: string): boolean {
    return this.lookup(name) !== undefined;
  }

  isDefinedLocally(name: string): boolean {
    return this.symbols.has(name);
  }

  // Types

  defineType(name: string, type: Type, kind: TypeDef["kind"]): void {
    this.types.set(name, { name, type, kind });
  }

  lookupType(name: string): TypeDef | undefined {
    const typeDef = this.types.get(name);
    if (typeDef) return typeDef;
    return this.parent?.lookupType(name);
  }

  // Iteration

  *allSymbols(): IterableIterator<Symbol> {
    yield* this.symbols.values();
    if (this.parent) {
      yield* this.parent.allSymbols();
    }
  }

  *allTypes(): IterableIterator<TypeDef> {
    yield* this.types.values();
    if (this.parent) {
      yield* this.parent.allTypes();
    }
  }
}

// =============================================================================
// TYPE ENVIRONMENT
// =============================================================================

export class TypeEnvironment {
  private currentScope: Scope;
  private globalScope: Scope;

  constructor() {
    this.globalScope = new Scope();
    this.currentScope = this.globalScope;
    this.initBuiltins();
  }

  private initBuiltins(): void {
    // Built-in types are handled specially during type resolution
    // No need to pre-register primitives
  }

  // Scope management

  pushScope(): void {
    this.currentScope = new Scope(this.currentScope);
  }

  popScope(): void {
    if (this.currentScope.parent) {
      this.currentScope = this.currentScope.parent;
    }
  }

  get scope(): Scope {
    return this.currentScope;
  }

  get global(): Scope {
    return this.globalScope;
  }

  // Convenience methods that delegate to current scope

  define(name: string, type: Type, mutable: boolean, kind: Symbol["kind"]): void {
    this.currentScope.define(name, type, mutable, kind);
  }

  lookup(name: string): Symbol | undefined {
    return this.currentScope.lookup(name);
  }

  defineType(name: string, type: Type, kind: TypeDef["kind"]): void {
    this.currentScope.defineType(name, type, kind);
  }

  lookupType(name: string): TypeDef | undefined {
    return this.currentScope.lookupType(name);
  }

  // Define a function in global scope
  defineFunction(name: string, type: FunctionType): void {
    this.globalScope.define(name, type, false, "function");
  }

  // Define a struct type in global scope
  defineStruct(name: string, type: StructType): void {
    this.globalScope.defineType(name, type, "struct");
  }

  // Define an enum type in global scope
  defineEnum(name: string, type: EnumType): void {
    this.globalScope.defineType(name, type, "enum");
  }

  // Run a function with a new scope, then pop it
  withScope<T>(fn: () => T): T {
    this.pushScope();
    try {
      return fn();
    } finally {
      this.popScope();
    }
  }
}

// =============================================================================
// SUBSTITUTION MAP (for type inference)
// =============================================================================

export class Substitution {
  private mappings: Map<number, Type> = new Map();

  set(varId: number, type: Type): void {
    this.mappings.set(varId, type);
  }

  get(varId: number): Type | undefined {
    return this.mappings.get(varId);
  }

  has(varId: number): boolean {
    return this.mappings.has(varId);
  }

  // Apply substitution to a type
  apply(type: Type): Type {
    switch (type.kind) {
      case "Primitive":
      case "Never":
      case "Unknown":
        return type;

      case "TypeVar": {
        const mapped = this.mappings.get(type.id);
        if (mapped) {
          // Recursively apply in case of chained substitutions
          return this.apply(mapped);
        }
        return type;
      }

      case "Function":
        return {
          kind: "Function",
          params: type.params.map((p) => this.apply(p)),
          ret: this.apply(type.ret),
        };

      case "Tuple":
        return {
          kind: "Tuple",
          elements: type.elements.map((e) => this.apply(e)),
        };

      case "Array":
        return {
          kind: "Array",
          element: this.apply(type.element),
        };

      case "Struct": {
        const newFields = new Map<string, Type>();
        for (const [name, fieldType] of type.fields) {
          newFields.set(name, this.apply(fieldType));
        }
        return {
          kind: "Struct",
          name: type.name,
          fields: newFields,
          typeParams: type.typeParams,
        };
      }

      case "Enum": {
        const newVariants = new Map<string, Type[] | null>();
        for (const [name, variantTypes] of type.variants) {
          if (variantTypes) {
            newVariants.set(name, variantTypes.map((vt) => this.apply(vt)));
          } else {
            newVariants.set(name, null);
          }
        }
        return {
          kind: "Enum",
          name: type.name,
          variants: newVariants,
          typeParams: type.typeParams,
        };
      }

      case "Generic":
        return {
          kind: "Generic",
          base: this.apply(type.base),
          args: type.args.map((a) => this.apply(a)),
        };
    }
  }

  // Compose two substitutions: apply s2 then s1
  compose(other: Substitution): Substitution {
    const result = new Substitution();

    // Apply this substitution to all mappings in other
    for (const [varId, type] of other.mappings) {
      result.set(varId, this.apply(type));
    }

    // Add mappings from this that aren't in other
    for (const [varId, type] of this.mappings) {
      if (!result.has(varId)) {
        result.set(varId, type);
      }
    }

    return result;
  }

  // Clone the substitution
  clone(): Substitution {
    const result = new Substitution();
    for (const [varId, type] of this.mappings) {
      result.set(varId, type);
    }
    return result;
  }

  // Debug: get all mappings
  entries(): IterableIterator<[number, Type]> {
    return this.mappings.entries();
  }
}
