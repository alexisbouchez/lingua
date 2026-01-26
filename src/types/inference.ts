/**
 * Lingua Type System - Type Inference
 *
 * Hindley-Milner style type inference with unification.
 */

import {
  type Type,
  type TypeVariable,
  containsTypeVar,
  freshTypeVar,
  typesEqual,
  typeToString,
} from "./types.ts";
import { Substitution } from "./environment.ts";

// =============================================================================
// UNIFICATION ERRORS
// =============================================================================

export class UnificationError extends Error {
  constructor(
    public readonly t1: Type,
    public readonly t2: Type,
    message?: string,
  ) {
    super(
      message ??
        `Cannot unify ${typeToString(t1)} with ${typeToString(t2)}`,
    );
    this.name = "UnificationError";
  }
}

export class OccursCheckError extends UnificationError {
  constructor(tv: TypeVariable, t: Type) {
    super(
      tv,
      t,
      `Infinite type: ${typeToString(tv)} occurs in ${typeToString(t)}`,
    );
    this.name = "OccursCheckError";
  }
}

// =============================================================================
// UNIFICATION
// =============================================================================

/**
 * Unify two types, returning a substitution that makes them equal.
 * Throws UnificationError if unification fails.
 */
export function unify(t1: Type, t2: Type, subst: Substitution = new Substitution()): Substitution {
  // Apply current substitution first
  const s1 = subst.apply(t1);
  const s2 = subst.apply(t2);

  // Same type? Done.
  if (typesEqual(s1, s2)) {
    return subst;
  }

  // Type variable on left?
  if (s1.kind === "TypeVar") {
    return unifyVar(s1, s2, subst);
  }

  // Type variable on right?
  if (s2.kind === "TypeVar") {
    return unifyVar(s2, s1, subst);
  }

  // Never type unifies with anything (it's the bottom type)
  if (s1.kind === "Never" || s2.kind === "Never") {
    return subst;
  }

  // Unknown type (from errors) unifies with anything
  if (s1.kind === "Unknown" || s2.kind === "Unknown") {
    return subst;
  }

  // Both are concrete types - must have same structure
  if (s1.kind !== s2.kind) {
    throw new UnificationError(s1, s2);
  }

  switch (s1.kind) {
    case "Primitive":
      // Already checked equality above
      throw new UnificationError(s1, s2);

    case "Function": {
      const f2 = s2 as typeof s1;
      if (s1.params.length !== f2.params.length) {
        throw new UnificationError(
          s1,
          s2,
          `Function arity mismatch: expected ${s1.params.length} params, got ${f2.params.length}`,
        );
      }
      let result = subst;
      for (let i = 0; i < s1.params.length; i++) {
        result = unify(s1.params[i], f2.params[i], result);
      }
      result = unify(s1.ret, f2.ret, result);
      return result;
    }

    case "Tuple": {
      const t2 = s2 as typeof s1;
      if (s1.elements.length !== t2.elements.length) {
        throw new UnificationError(
          s1,
          s2,
          `Tuple length mismatch: expected ${s1.elements.length}, got ${t2.elements.length}`,
        );
      }
      let result = subst;
      for (let i = 0; i < s1.elements.length; i++) {
        result = unify(s1.elements[i], t2.elements[i], result);
      }
      return result;
    }

    case "Array": {
      const a2 = s2 as typeof s1;
      return unify(s1.element, a2.element, subst);
    }

    case "Struct": {
      const st2 = s2 as typeof s1;
      if (s1.name !== st2.name) {
        throw new UnificationError(s1, s2);
      }
      // Struct types are nominal - same name means same type
      // But we still need to unify type parameters if any
      return subst;
    }

    case "Enum": {
      const e2 = s2 as typeof s1;
      if (s1.name !== e2.name) {
        throw new UnificationError(s1, s2);
      }
      return subst;
    }

    case "Generic": {
      const g2 = s2 as typeof s1;
      if (s1.args.length !== g2.args.length) {
        throw new UnificationError(s1, s2);
      }
      let result = unify(s1.base, g2.base, subst);
      for (let i = 0; i < s1.args.length; i++) {
        result = unify(s1.args[i], g2.args[i], result);
      }
      return result;
    }

    default:
      throw new UnificationError(s1, s2);
  }
}

/**
 * Unify a type variable with a type.
 */
function unifyVar(tv: TypeVariable, t: Type, subst: Substitution): Substitution {
  // Already bound?
  const existing = subst.get(tv.id);
  if (existing) {
    return unify(existing, t, subst);
  }

  // t is also a type variable that's bound?
  if (t.kind === "TypeVar") {
    const tBound = subst.get(t.id);
    if (tBound) {
      return unify(tv, tBound, subst);
    }
  }

  // Occurs check: tv cannot appear in t
  const appliedT = subst.apply(t);
  if (containsTypeVar(appliedT, tv.id)) {
    throw new OccursCheckError(tv, appliedT);
  }

  // Bind tv to t
  const result = subst.clone();
  result.set(tv.id, appliedT);
  return result;
}

// =============================================================================
// CONSTRAINT-BASED INFERENCE
// =============================================================================

export interface Constraint {
  left: Type;
  right: Type;
  message?: string;
}

export class ConstraintSet {
  private constraints: Constraint[] = [];

  add(left: Type, right: Type, message?: string): void {
    this.constraints.push({ left, right, message });
  }

  addEqual(t1: Type, t2: Type, message?: string): void {
    this.add(t1, t2, message);
  }

  /**
   * Solve all constraints, returning a substitution.
   * Throws UnificationError if constraints are unsatisfiable.
   */
  solve(): Substitution {
    let subst = new Substitution();

    for (const constraint of this.constraints) {
      try {
        subst = unify(constraint.left, constraint.right, subst);
      } catch (e) {
        if (e instanceof UnificationError && constraint.message) {
          throw new UnificationError(
            constraint.left,
            constraint.right,
            `${constraint.message}: ${e.message}`,
          );
        }
        throw e;
      }
    }

    return subst;
  }

  // Debug
  get length(): number {
    return this.constraints.length;
  }

  *[Symbol.iterator](): IterableIterator<Constraint> {
    yield* this.constraints;
  }
}

// =============================================================================
// INSTANTIATION & GENERALIZATION
// =============================================================================

/**
 * Instantiate a polymorphic type by replacing its type variables
 * with fresh type variables.
 */
export function instantiate(type: Type, typeVars: TypeVariable[]): {
  type: Type;
  mapping: Map<number, TypeVariable>;
} {
  const mapping = new Map<number, TypeVariable>();

  for (const tv of typeVars) {
    mapping.set(tv.id, freshTypeVar(tv.name));
  }

  function inst(t: Type): Type {
    switch (t.kind) {
      case "Primitive":
      case "Never":
      case "Unknown":
        return t;

      case "TypeVar": {
        const replacement = mapping.get(t.id);
        return replacement ?? t;
      }

      case "Function":
        return {
          kind: "Function",
          params: t.params.map(inst),
          ret: inst(t.ret),
        };

      case "Tuple":
        return {
          kind: "Tuple",
          elements: t.elements.map(inst),
        };

      case "Array":
        return {
          kind: "Array",
          element: inst(t.element),
        };

      case "Struct": {
        const newFields = new Map<string, Type>();
        for (const [name, fieldType] of t.fields) {
          newFields.set(name, inst(fieldType));
        }
        return {
          kind: "Struct",
          name: t.name,
          fields: newFields,
          typeParams: t.typeParams?.map((tv) => mapping.get(tv.id) ?? tv),
        };
      }

      case "Enum": {
        const newVariants = new Map<string, Type[] | null>();
        for (const [name, variantTypes] of t.variants) {
          if (variantTypes) {
            newVariants.set(name, variantTypes.map(inst));
          } else {
            newVariants.set(name, null);
          }
        }
        return {
          kind: "Enum",
          name: t.name,
          variants: newVariants,
          typeParams: t.typeParams?.map((tv) => mapping.get(tv.id) ?? tv),
        };
      }

      case "Generic":
        return {
          kind: "Generic",
          base: inst(t.base),
          args: t.args.map(inst),
        };
    }
  }

  return { type: inst(type), mapping };
}

/**
 * Find all free type variables in a type.
 */
export function freeTypeVars(type: Type): Set<number> {
  const result = new Set<number>();

  function collect(t: Type): void {
    switch (t.kind) {
      case "Primitive":
      case "Never":
      case "Unknown":
        break;

      case "TypeVar":
        result.add(t.id);
        break;

      case "Function":
        t.params.forEach(collect);
        collect(t.ret);
        break;

      case "Tuple":
        t.elements.forEach(collect);
        break;

      case "Array":
        collect(t.element);
        break;

      case "Struct":
        for (const fieldType of t.fields.values()) {
          collect(fieldType);
        }
        break;

      case "Enum":
        for (const variantTypes of t.variants.values()) {
          if (variantTypes) {
            variantTypes.forEach(collect);
          }
        }
        break;

      case "Generic":
        collect(t.base);
        t.args.forEach(collect);
        break;
    }
  }

  collect(type);
  return result;
}
