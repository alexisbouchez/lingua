/**
 * Type System Tests
 */

import { assertEquals, assertThrows } from "jsr:@std/assert";
import { describe, it, beforeEach } from "jsr:@std/testing/bdd";
import {
  Type,
  primitive,
  functionType,
  tupleType,
  arrayType,
  structType,
  enumType,
  freshTypeVar,
  resetTypeVarCounter,
  typeToString,
  typesEqual,
  containsTypeVar,
  isNumeric,
  isInteger,
  isFloat,
  BOOL,
  I32,
  I64,
  F32,
  F64,
  STR,
  UNIT,
  NEVER,
  UNKNOWN,
} from "./types.ts";
import { TypeEnvironment, Substitution, Scope } from "./environment.ts";
import { unify, UnificationError, OccursCheckError, ConstraintSet, freeTypeVars, instantiate } from "./inference.ts";

// =============================================================================
// TYPE REPRESENTATION TESTS
// =============================================================================

describe("Type Representation", () => {
  beforeEach(() => {
    resetTypeVarCounter();
  });

  describe("Primitives", () => {
    it("creates primitive types", () => {
      assertEquals(primitive("i32"), { kind: "Primitive", name: "i32" });
      assertEquals(primitive("bool"), { kind: "Primitive", name: "bool" });
      assertEquals(primitive("str"), { kind: "Primitive", name: "str" });
    });

    it("has predefined primitives", () => {
      assertEquals(I32, { kind: "Primitive", name: "i32" });
      assertEquals(BOOL, { kind: "Primitive", name: "bool" });
      assertEquals(UNIT, { kind: "Primitive", name: "unit" });
    });
  });

  describe("Function Types", () => {
    it("creates function types", () => {
      const fn = functionType([I32, I32], I32);
      assertEquals(fn.kind, "Function");
      assertEquals(fn.params.length, 2);
      assertEquals(fn.ret, I32);
    });

    it("creates nullary function types", () => {
      const fn = functionType([], UNIT);
      assertEquals(fn.params.length, 0);
      assertEquals(fn.ret, UNIT);
    });
  });

  describe("Tuple Types", () => {
    it("creates tuple types", () => {
      const tup = tupleType([I32, STR, BOOL]);
      assertEquals(tup.kind, "Tuple");
      assertEquals(tup.elements.length, 3);
    });

    it("creates empty tuple (unit)", () => {
      const tup = tupleType([]);
      assertEquals(tup.elements.length, 0);
    });
  });

  describe("Array Types", () => {
    it("creates array types", () => {
      const arr = arrayType(I32);
      assertEquals(arr.kind, "Array");
      assertEquals(arr.element, I32);
    });
  });

  describe("Struct Types", () => {
    it("creates struct types", () => {
      const fields = new Map<string, Type>([
        ["x", I32],
        ["y", I32],
      ]);
      const st = structType("Point", fields);
      assertEquals(st.kind, "Struct");
      assertEquals(st.name, "Point");
      assertEquals(st.fields.get("x"), I32);
    });
  });

  describe("Enum Types", () => {
    it("creates enum types", () => {
      const variants = new Map<string, Type[] | null>([
        ["None", null],
        ["Some", [I32]],
      ]);
      const en = enumType("Option", variants);
      assertEquals(en.kind, "Enum");
      assertEquals(en.name, "Option");
      assertEquals(en.variants.get("None"), null);
      assertEquals(en.variants.get("Some"), [I32]);
    });
  });

  describe("Type Variables", () => {
    it("creates fresh type variables", () => {
      const tv1 = freshTypeVar();
      const tv2 = freshTypeVar();
      assertEquals(tv1.kind, "TypeVar");
      assertEquals(tv1.id, 0);
      assertEquals(tv2.id, 1);
    });

    it("creates named type variables", () => {
      const tv = freshTypeVar("T");
      assertEquals(tv.name, "T");
    });
  });

  describe("typeToString", () => {
    it("formats primitives", () => {
      assertEquals(typeToString(I32), "i32");
      assertEquals(typeToString(BOOL), "bool");
      assertEquals(typeToString(UNIT), "()");
    });

    it("formats functions", () => {
      const fn = functionType([I32, I32], I32);
      assertEquals(typeToString(fn), "fn(i32, i32) -> i32");
    });

    it("formats tuples", () => {
      const tup = tupleType([I32, STR]);
      assertEquals(typeToString(tup), "(i32, str)");
    });

    it("formats arrays", () => {
      const arr = arrayType(I32);
      assertEquals(typeToString(arr), "[i32]");
    });

    it("formats type variables", () => {
      const tv = freshTypeVar("T");
      assertEquals(typeToString(tv), "T");

      const unnamed = freshTypeVar();
      assertEquals(typeToString(unnamed), "?T1");
    });

    it("formats special types", () => {
      assertEquals(typeToString(NEVER), "!");
      assertEquals(typeToString(UNKNOWN), "?");
    });
  });

  describe("typesEqual", () => {
    it("compares primitives", () => {
      assertEquals(typesEqual(I32, I32), true);
      assertEquals(typesEqual(I32, I64), false);
    });

    it("compares functions", () => {
      const fn1 = functionType([I32], I32);
      const fn2 = functionType([I32], I32);
      const fn3 = functionType([I64], I32);
      assertEquals(typesEqual(fn1, fn2), true);
      assertEquals(typesEqual(fn1, fn3), false);
    });

    it("compares tuples", () => {
      const t1 = tupleType([I32, STR]);
      const t2 = tupleType([I32, STR]);
      const t3 = tupleType([I32, I32]);
      assertEquals(typesEqual(t1, t2), true);
      assertEquals(typesEqual(t1, t3), false);
    });

    it("compares type variables by id", () => {
      const tv1 = freshTypeVar();
      const tv2 = freshTypeVar();
      assertEquals(typesEqual(tv1, tv1), true);
      assertEquals(typesEqual(tv1, tv2), false);
    });
  });

  describe("containsTypeVar", () => {
    it("detects type variables", () => {
      const tv = freshTypeVar();
      assertEquals(containsTypeVar(tv, tv.id), true);
      assertEquals(containsTypeVar(I32, tv.id), false);
    });

    it("searches in nested types", () => {
      const tv = freshTypeVar();
      const fn = functionType([tv], I32);
      assertEquals(containsTypeVar(fn, tv.id), true);

      const arr = arrayType(tv);
      assertEquals(containsTypeVar(arr, tv.id), true);
    });
  });

  describe("isNumeric/isInteger/isFloat", () => {
    it("identifies numeric types", () => {
      assertEquals(isNumeric(I32), true);
      assertEquals(isNumeric(F64), true);
      assertEquals(isNumeric(BOOL), false);
    });

    it("identifies integer types", () => {
      assertEquals(isInteger(I32), true);
      assertEquals(isInteger(F64), false);
    });

    it("identifies float types", () => {
      assertEquals(isFloat(F32), true);
      assertEquals(isFloat(I32), false);
    });
  });
});

// =============================================================================
// ENVIRONMENT TESTS
// =============================================================================

describe("Type Environment", () => {
  describe("Scope", () => {
    it("defines and looks up variables", () => {
      const scope = new Scope();
      scope.define("x", I32, false, "variable");
      const sym = scope.lookup("x");
      assertEquals(sym?.name, "x");
      assertEquals(sym?.type, I32);
      assertEquals(sym?.mutable, false);
    });

    it("shadows parent scope", () => {
      const parent = new Scope();
      parent.define("x", I32, false, "variable");

      const child = new Scope(parent);
      child.define("x", STR, false, "variable");

      assertEquals(child.lookup("x")?.type, STR);
      assertEquals(parent.lookup("x")?.type, I32);
    });

    it("looks up in parent scope", () => {
      const parent = new Scope();
      parent.define("x", I32, false, "variable");

      const child = new Scope(parent);
      assertEquals(child.lookup("x")?.type, I32);
    });

    it("defines and looks up types", () => {
      const scope = new Scope();
      const pointType = structType("Point", new Map([["x", I32], ["y", I32]]));
      scope.defineType("Point", pointType, "struct");

      const td = scope.lookupType("Point");
      assertEquals(td?.name, "Point");
      assertEquals(td?.kind, "struct");
    });
  });

  describe("TypeEnvironment", () => {
    it("manages scopes", () => {
      const env = new TypeEnvironment();
      env.define("x", I32, false, "variable");
      assertEquals(env.lookup("x")?.type, I32);

      env.pushScope();
      env.define("y", STR, false, "variable");
      assertEquals(env.lookup("y")?.type, STR);
      assertEquals(env.lookup("x")?.type, I32);

      env.popScope();
      assertEquals(env.lookup("y"), undefined);
      assertEquals(env.lookup("x")?.type, I32);
    });

    it("defines functions in global scope", () => {
      const env = new TypeEnvironment();
      const fnType = functionType([I32], I32);
      env.defineFunction("square", fnType);

      env.pushScope();
      env.pushScope();
      assertEquals(env.lookup("square")?.type, fnType);
    });

    it("withScope helper", () => {
      const env = new TypeEnvironment();
      env.define("x", I32, false, "variable");

      const result = env.withScope(() => {
        env.define("y", STR, false, "variable");
        assertEquals(env.lookup("y")?.type, STR);
        return 42;
      });

      assertEquals(result, 42);
      assertEquals(env.lookup("y"), undefined);
    });
  });

  describe("Substitution", () => {
    beforeEach(() => {
      resetTypeVarCounter();
    });

    it("applies substitution to type variables", () => {
      const subst = new Substitution();
      const tv = freshTypeVar();
      subst.set(tv.id, I32);

      assertEquals(subst.apply(tv), I32);
    });

    it("applies recursively through type structures", () => {
      const subst = new Substitution();
      const tv = freshTypeVar();
      subst.set(tv.id, I32);

      const fn = functionType([tv], tv);
      const applied = subst.apply(fn);

      assertEquals(applied.kind, "Function");
      if (applied.kind === "Function") {
        assertEquals(applied.params[0], I32);
        assertEquals(applied.ret, I32);
      }
    });

    it("handles chained substitutions", () => {
      const subst = new Substitution();
      const tv1 = freshTypeVar();
      const tv2 = freshTypeVar();

      subst.set(tv1.id, tv2);
      subst.set(tv2.id, I32);

      assertEquals(subst.apply(tv1), I32);
    });

    it("composes substitutions", () => {
      const s1 = new Substitution();
      const s2 = new Substitution();
      const tv1 = freshTypeVar();
      const tv2 = freshTypeVar();

      s1.set(tv1.id, I32);
      s2.set(tv2.id, tv1);

      const composed = s1.compose(s2);
      assertEquals(composed.apply(tv2), I32);
    });
  });
});

// =============================================================================
// UNIFICATION TESTS
// =============================================================================

describe("Unification", () => {
  beforeEach(() => {
    resetTypeVarCounter();
  });

  describe("Basic Unification", () => {
    it("unifies identical types", () => {
      const subst = unify(I32, I32);
      assertEquals([...subst.entries()].length, 0);
    });

    it("unifies type variable with concrete type", () => {
      const tv = freshTypeVar();
      const subst = unify(tv, I32);
      assertEquals(subst.apply(tv), I32);
    });

    it("unifies two type variables", () => {
      const tv1 = freshTypeVar();
      const tv2 = freshTypeVar();
      const subst = unify(tv1, tv2);

      // One should map to the other
      const applied1 = subst.apply(tv1);
      const applied2 = subst.apply(tv2);
      assertEquals(typesEqual(applied1, applied2), true);
    });

    it("fails on incompatible primitives", () => {
      assertThrows(() => unify(I32, STR), UnificationError);
    });
  });

  describe("Function Unification", () => {
    it("unifies compatible functions", () => {
      const tv = freshTypeVar();
      const fn1 = functionType([tv], tv);
      const fn2 = functionType([I32], I32);

      const subst = unify(fn1, fn2);
      assertEquals(subst.apply(tv), I32);
    });

    it("fails on arity mismatch", () => {
      const fn1 = functionType([I32], I32);
      const fn2 = functionType([I32, I32], I32);
      assertThrows(() => unify(fn1, fn2), UnificationError);
    });

    it("fails on return type mismatch", () => {
      const fn1 = functionType([I32], I32);
      const fn2 = functionType([I32], STR);
      assertThrows(() => unify(fn1, fn2), UnificationError);
    });
  });

  describe("Tuple Unification", () => {
    it("unifies compatible tuples", () => {
      const tv = freshTypeVar();
      const t1 = tupleType([tv, I32]);
      const t2 = tupleType([STR, I32]);

      const subst = unify(t1, t2);
      assertEquals(subst.apply(tv), STR);
    });

    it("fails on length mismatch", () => {
      const t1 = tupleType([I32, I32]);
      const t2 = tupleType([I32]);
      assertThrows(() => unify(t1, t2), UnificationError);
    });
  });

  describe("Array Unification", () => {
    it("unifies compatible arrays", () => {
      const tv = freshTypeVar();
      const a1 = arrayType(tv);
      const a2 = arrayType(I32);

      const subst = unify(a1, a2);
      assertEquals(subst.apply(tv), I32);
    });
  });

  describe("Occurs Check", () => {
    it("prevents infinite types", () => {
      const tv = freshTypeVar();
      const fn = functionType([tv], tv);

      // Trying to unify tv with fn(tv) -> tv would create infinite type
      assertThrows(() => unify(tv, fn), OccursCheckError);
    });
  });

  describe("Special Types", () => {
    it("Never unifies with anything", () => {
      const subst = unify(NEVER, I32);
      assertEquals([...subst.entries()].length, 0);
    });

    it("Unknown unifies with anything", () => {
      const subst = unify(UNKNOWN, I32);
      assertEquals([...subst.entries()].length, 0);
    });
  });
});

// =============================================================================
// CONSTRAINT SET TESTS
// =============================================================================

describe("ConstraintSet", () => {
  beforeEach(() => {
    resetTypeVarCounter();
  });

  it("solves simple constraints", () => {
    const cs = new ConstraintSet();
    const tv = freshTypeVar();

    cs.add(tv, I32);

    const subst = cs.solve();
    assertEquals(subst.apply(tv), I32);
  });

  it("solves multiple constraints", () => {
    const cs = new ConstraintSet();
    const tv1 = freshTypeVar();
    const tv2 = freshTypeVar();

    cs.add(tv1, I32);
    cs.add(tv2, tv1);

    const subst = cs.solve();
    assertEquals(subst.apply(tv1), I32);
    assertEquals(subst.apply(tv2), I32);
  });

  it("solves transitive constraints", () => {
    const cs = new ConstraintSet();
    const tv1 = freshTypeVar();
    const tv2 = freshTypeVar();
    const tv3 = freshTypeVar();

    cs.add(tv1, tv2);
    cs.add(tv2, tv3);
    cs.add(tv3, I32);

    const subst = cs.solve();
    assertEquals(subst.apply(tv1), I32);
  });

  it("throws on unsatisfiable constraints", () => {
    const cs = new ConstraintSet();
    const tv = freshTypeVar();

    cs.add(tv, I32);
    cs.add(tv, STR);

    assertThrows(() => cs.solve(), UnificationError);
  });
});

// =============================================================================
// INSTANTIATION TESTS
// =============================================================================

describe("Instantiation", () => {
  beforeEach(() => {
    resetTypeVarCounter();
  });

  it("instantiates polymorphic types", () => {
    const tv = freshTypeVar("T");
    const fn = functionType([tv], tv);

    const { type, mapping } = instantiate(fn, [tv]);

    // Should have created a fresh type variable
    assertEquals(type.kind, "Function");
    if (type.kind === "Function") {
      assertEquals(type.params[0].kind, "TypeVar");
      assertEquals(type.ret.kind, "TypeVar");
      // The new variable should be different from the original
      if (type.params[0].kind === "TypeVar") {
        assertEquals(type.params[0].id !== tv.id, true);
      }
    }
  });
});

// =============================================================================
// FREE TYPE VARIABLES TESTS
// =============================================================================

describe("freeTypeVars", () => {
  beforeEach(() => {
    resetTypeVarCounter();
  });

  it("finds type variables in simple types", () => {
    const tv = freshTypeVar();
    const ftv = freeTypeVars(tv);
    assertEquals(ftv.has(tv.id), true);
  });

  it("returns empty for concrete types", () => {
    const ftv = freeTypeVars(I32);
    assertEquals(ftv.size, 0);
  });

  it("finds type variables in functions", () => {
    const tv1 = freshTypeVar();
    const tv2 = freshTypeVar();
    const fn = functionType([tv1], tv2);

    const ftv = freeTypeVars(fn);
    assertEquals(ftv.has(tv1.id), true);
    assertEquals(ftv.has(tv2.id), true);
  });

  it("finds type variables in nested structures", () => {
    const tv = freshTypeVar();
    const arr = arrayType(tupleType([tv, I32]));

    const ftv = freeTypeVars(arr);
    assertEquals(ftv.has(tv.id), true);
    assertEquals(ftv.size, 1);
  });
});
