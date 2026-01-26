/**
 * Lingua Type System - Type Checker
 *
 * Walks the AST and performs type checking with inference.
 */

import type * as ast from "../parser/ast.ts";
import type { Span } from "../lexer/token.ts";
import {
  type Type,
  type TypeVariable,
  type PrimitiveName,
  BOOL,
  UNIT,
  I32,
  I64,
  F32,
  F64,
  STR,
  CHAR,
  NEVER,
  UNKNOWN,
  primitive,
  functionType,
  tupleType,
  arrayType,
  structType,
  enumType,
  freshTypeVar,
  typeToString,
  isNumeric,
  isInteger,
  isFloat,
  typesEqual,
  resetTypeVarCounter,
} from "./types.ts";
import { TypeEnvironment, Substitution } from "./environment.ts";
import { unify, UnificationError, ConstraintSet, freeTypeVars, instantiate } from "./inference.ts";

// =============================================================================
// TYPE ERRORS
// =============================================================================

export interface TypeError {
  message: string;
  span: Span;
  notes?: string[];
}

// =============================================================================
// TYPE CHECKER
// =============================================================================

export class TypeChecker {
  private env: TypeEnvironment;
  private errors: TypeError[] = [];
  private constraints: ConstraintSet = new ConstraintSet();
  private currentReturnType: Type | null = null;

  constructor() {
    this.env = new TypeEnvironment();
  }

  // ---------------------------------------------------------------------------
  // PUBLIC API
  // ---------------------------------------------------------------------------

  check(module: ast.Module): { errors: TypeError[]; types: Map<ast.Node, Type> } {
    resetTypeVarCounter();
    this.errors = [];
    this.constraints = new ConstraintSet();

    // First pass: collect all type definitions (structs, enums)
    for (const item of module.items) {
      this.collectTypeDef(item);
    }

    // Second pass: collect all function signatures
    for (const item of module.items) {
      this.collectFunctionSig(item);
    }

    // Third pass: type check function bodies and expressions
    for (const item of module.items) {
      this.checkItem(item);
    }

    // Solve constraints
    let subst: Substitution;
    try {
      subst = this.constraints.solve();
    } catch (e) {
      if (e instanceof UnificationError) {
        this.error({
          message: e.message,
          span: module.span,
        });
      }
      subst = new Substitution();
    }

    // TODO: Apply substitution to collect final types
    const types = new Map<ast.Node, Type>();

    return { errors: this.errors, types };
  }

  // ---------------------------------------------------------------------------
  // TYPE DEFINITION COLLECTION (Pass 1)
  // ---------------------------------------------------------------------------

  private collectTypeDef(item: ast.Item): void {
    switch (item.kind) {
      case "StructItem":
        this.collectStruct(item);
        break;
      case "EnumItem":
        this.collectEnum(item);
        break;
      case "TypeItem":
        this.collectTypeAlias(item);
        break;
    }
  }

  private collectStruct(item: ast.StructItem): void {
    const typeParams: TypeVariable[] = [];
    if (item.generics) {
      for (const param of item.generics.params) {
        typeParams.push(freshTypeVar(param.name));
      }
    }

    const fields = new Map<string, Type>();
    for (const field of item.fields) {
      const fieldType = this.resolveAstType(field.type, typeParams);
      fields.set(field.name, fieldType);
    }

    const type = structType(item.name, fields, typeParams.length > 0 ? typeParams : undefined);
    this.env.defineStruct(item.name, type);
  }

  private collectEnum(item: ast.EnumItem): void {
    const typeParams: TypeVariable[] = [];
    if (item.generics) {
      for (const param of item.generics.params) {
        typeParams.push(freshTypeVar(param.name));
      }
    }

    const variants = new Map<string, Type[] | null>();
    for (const variant of item.variants) {
      if (variant.fields) {
        const fieldTypes = variant.fields.map((t) => this.resolveAstType(t, typeParams));
        variants.set(variant.name, fieldTypes);
      } else if (variant.named) {
        // Struct variant - for now treat as tuple of field types
        const fieldTypes = variant.named.map((f) => this.resolveAstType(f.type, typeParams));
        variants.set(variant.name, fieldTypes);
      } else {
        variants.set(variant.name, null);
      }
    }

    const type = enumType(item.name, variants, typeParams.length > 0 ? typeParams : undefined);
    this.env.defineEnum(item.name, type);
  }

  private collectTypeAlias(item: ast.TypeItem): void {
    if (!item.type) return;

    const typeParams: TypeVariable[] = [];
    if (item.generics) {
      for (const param of item.generics.params) {
        typeParams.push(freshTypeVar(param.name));
      }
    }

    const type = this.resolveAstType(item.type, typeParams);
    this.env.defineType(item.name, type, "alias");
  }

  // ---------------------------------------------------------------------------
  // FUNCTION SIGNATURE COLLECTION (Pass 2)
  // ---------------------------------------------------------------------------

  private collectFunctionSig(item: ast.Item): void {
    if (item.kind !== "FnItem") return;

    const typeParams: TypeVariable[] = [];
    if (item.generics) {
      for (const param of item.generics.params) {
        typeParams.push(freshTypeVar(param.name));
      }
    }

    const paramTypes: Type[] = [];
    for (const param of item.params) {
      paramTypes.push(this.resolveAstType(param.type, typeParams));
    }

    const retType = item.ret ? this.resolveAstType(item.ret, typeParams) : UNIT;
    const fnType = functionType(paramTypes, retType);

    this.env.defineFunction(item.name, fnType);
  }

  // ---------------------------------------------------------------------------
  // ITEM TYPE CHECKING (Pass 3)
  // ---------------------------------------------------------------------------

  private checkItem(item: ast.Item): void {
    switch (item.kind) {
      case "FnItem":
        this.checkFunction(item);
        break;
      case "ImplItem":
        this.checkImpl(item);
        break;
    }
  }

  private checkFunction(fn: ast.FnItem): void {
    if (!fn.body) return; // Declaration only

    this.env.pushScope();

    // Bind type parameters
    const typeParams: TypeVariable[] = [];
    if (fn.generics) {
      for (const param of fn.generics.params) {
        const tv = freshTypeVar(param.name);
        typeParams.push(tv);
      }
    }

    // Bind parameters
    for (const param of fn.params) {
      const paramType = this.resolveAstType(param.type, typeParams);
      this.bindPattern(param.pattern, paramType, false);
    }

    // Set expected return type
    const retType = fn.ret ? this.resolveAstType(fn.ret, typeParams) : UNIT;
    this.currentReturnType = retType;

    // Check body
    const bodyType = this.inferExpr(fn.body);

    // Body type must match return type
    this.addConstraint(bodyType, retType, `Function '${fn.name}' body type mismatch`, fn.body.span);

    this.currentReturnType = null;
    this.env.popScope();
  }

  private checkImpl(impl: ast.ImplItem): void {
    // TODO: Full impl checking with traits
    for (const method of impl.items) {
      this.checkFunction(method);
    }
  }

  // ---------------------------------------------------------------------------
  // EXPRESSION TYPE INFERENCE
  // ---------------------------------------------------------------------------

  inferExpr(expr: ast.Expr): Type {
    switch (expr.kind) {
      case "IntLiteral":
        return this.inferIntLiteral(expr);
      case "FloatLiteral":
        return F64;
      case "StringLiteral":
        return STR;
      case "CharLiteral":
        return CHAR;
      case "BoolLiteral":
        return BOOL;
      case "Identifier":
        return this.inferIdentifier(expr);
      case "BinaryExpr":
        return this.inferBinaryExpr(expr);
      case "UnaryExpr":
        return this.inferUnaryExpr(expr);
      case "CallExpr":
        return this.inferCallExpr(expr);
      case "MethodCallExpr":
        return this.inferMethodCallExpr(expr);
      case "FieldExpr":
        return this.inferFieldExpr(expr);
      case "IndexExpr":
        return this.inferIndexExpr(expr);
      case "TupleExpr":
        return this.inferTupleExpr(expr);
      case "ArrayExpr":
        return this.inferArrayExpr(expr);
      case "StructExpr":
        return this.inferStructExpr(expr);
      case "IfExpr":
        return this.inferIfExpr(expr);
      case "MatchExpr":
        return this.inferMatchExpr(expr);
      case "BlockExpr":
        return this.inferBlockExpr(expr);
      case "ClosureExpr":
        return this.inferClosureExpr(expr);
      case "ReturnExpr":
        return this.inferReturnExpr(expr);
      case "BreakExpr":
      case "ContinueExpr":
        return NEVER;
      case "LoopExpr":
        return this.inferLoopExpr(expr);
      case "WhileExpr":
        return this.inferWhileExpr(expr);
      case "ForExpr":
        return this.inferForExpr(expr);
      case "RangeExpr":
        return this.inferRangeExpr(expr);
      case "AssignExpr":
        return this.inferAssignExpr(expr);
      case "TryExpr":
        return this.inferTryExpr(expr);
      case "AwaitExpr":
        return this.inferAwaitExpr(expr);
      case "PathExpr":
        return this.inferPathExpr(expr);
      default:
        return UNKNOWN;
    }
  }

  private inferIntLiteral(_expr: ast.IntLiteral): Type {
    // Default to i32, but could be polymorphic
    return I32;
  }

  private inferIdentifier(expr: ast.Identifier): Type {
    const symbol = this.env.lookup(expr.name);
    if (!symbol) {
      this.error({
        message: `Undefined variable: ${expr.name}`,
        span: expr.span,
      });
      return UNKNOWN;
    }

    // If it's a polymorphic function, instantiate it
    if (symbol.kind === "function" && symbol.type.kind === "Function") {
      const ftv = freeTypeVars(symbol.type);
      if (ftv.size > 0) {
        const typeVars = Array.from(ftv).map((id) => ({ kind: "TypeVar" as const, id }));
        const { type } = instantiate(symbol.type, typeVars);
        return type;
      }
    }

    return symbol.type;
  }

  private inferBinaryExpr(expr: ast.BinaryExpr): Type {
    const leftType = this.inferExpr(expr.left);
    const rightType = this.inferExpr(expr.right);

    switch (expr.op) {
      // Arithmetic: both operands must be same numeric type
      case "+":
      case "-":
      case "*":
      case "/":
      case "%":
        this.addConstraint(leftType, rightType, "Binary operands must have same type", expr.span);
        if (!isNumeric(leftType) && leftType.kind !== "TypeVar") {
          this.error({
            message: `Operator '${expr.op}' requires numeric operands, got ${typeToString(leftType)}`,
            span: expr.span,
          });
        }
        return leftType;

      // Comparison: both operands same type, result is bool
      case "==":
      case "!=":
      case "<":
      case ">":
      case "<=":
      case ">=":
        this.addConstraint(leftType, rightType, "Comparison operands must have same type", expr.span);
        return BOOL;

      // Logical: both must be bool
      case "&&":
      case "||":
        this.addConstraint(leftType, BOOL, "Logical operator requires bool", expr.span);
        this.addConstraint(rightType, BOOL, "Logical operator requires bool", expr.span);
        return BOOL;

      // Bitwise: both must be integer
      case "&":
      case "|":
      case "^":
      case "<<":
      case ">>":
        this.addConstraint(leftType, rightType, "Bitwise operands must have same type", expr.span);
        if (!isInteger(leftType) && leftType.kind !== "TypeVar") {
          this.error({
            message: `Operator '${expr.op}' requires integer operands`,
            span: expr.span,
          });
        }
        return leftType;

      default:
        return UNKNOWN;
    }
  }

  private inferUnaryExpr(expr: ast.UnaryExpr): Type {
    const innerType = this.inferExpr(expr.expr);

    switch (expr.op) {
      case "-":
        if (!isNumeric(innerType) && innerType.kind !== "TypeVar") {
          this.error({
            message: `Unary '-' requires numeric operand, got ${typeToString(innerType)}`,
            span: expr.span,
          });
        }
        return innerType;

      case "!":
        this.addConstraint(innerType, BOOL, "Logical not requires bool", expr.span);
        return BOOL;

      case "&":
        // Reference - for now just return the type
        return innerType;

      case "*":
        // Dereference - for now just return the type
        return innerType;

      default:
        return UNKNOWN;
    }
  }

  private inferCallExpr(expr: ast.CallExpr): Type {
    const calleeType = this.inferExpr(expr.callee);

    if (calleeType.kind !== "Function") {
      if (calleeType.kind !== "TypeVar" && calleeType.kind !== "Unknown") {
        this.error({
          message: `Cannot call non-function type ${typeToString(calleeType)}`,
          span: expr.span,
        });
      }
      // For type variables, create a function type constraint
      if (calleeType.kind === "TypeVar") {
        const paramTypes = expr.args.map(() => freshTypeVar());
        const retType = freshTypeVar();
        const expectedFnType = functionType(paramTypes, retType);
        this.addConstraint(calleeType, expectedFnType, "Expected function type", expr.span);

        // Check arguments
        for (let i = 0; i < expr.args.length; i++) {
          const argType = this.inferExpr(expr.args[i]);
          this.addConstraint(argType, paramTypes[i], `Argument ${i + 1} type mismatch`, expr.args[i].span);
        }

        return retType;
      }
      return UNKNOWN;
    }

    // Check argument count
    if (expr.args.length !== calleeType.params.length) {
      this.error({
        message: `Expected ${calleeType.params.length} arguments, got ${expr.args.length}`,
        span: expr.span,
      });
    }

    // Check argument types
    const argCount = Math.min(expr.args.length, calleeType.params.length);
    for (let i = 0; i < argCount; i++) {
      const argType = this.inferExpr(expr.args[i]);
      this.addConstraint(argType, calleeType.params[i], `Argument ${i + 1} type mismatch`, expr.args[i].span);
    }

    return calleeType.ret;
  }

  private inferMethodCallExpr(expr: ast.MethodCallExpr): Type {
    const receiverType = this.inferExpr(expr.receiver);
    // TODO: Method resolution
    // For now, return unknown
    return UNKNOWN;
  }

  private inferFieldExpr(expr: ast.FieldExpr): Type {
    const objType = this.inferExpr(expr.expr);

    // Handle tuple field access (e.g., tuple.0)
    if (objType.kind === "Tuple") {
      const index = parseInt(expr.field);
      if (!isNaN(index)) {
        if (index < 0 || index >= objType.elements.length) {
          this.error({
            message: `Tuple index ${index} out of bounds (tuple has ${objType.elements.length} elements)`,
            span: expr.span,
          });
          return UNKNOWN;
        }
        return objType.elements[index];
      }
    }

    // Handle struct field access
    if (objType.kind === "Struct") {
      const fieldType = objType.fields.get(expr.field);
      if (!fieldType) {
        this.error({
          message: `Struct '${objType.name}' has no field '${expr.field}'`,
          span: expr.span,
        });
        return UNKNOWN;
      }
      return fieldType;
    }

    if (objType.kind !== "TypeVar" && objType.kind !== "Unknown") {
      this.error({
        message: `Cannot access field '${expr.field}' on type ${typeToString(objType)}`,
        span: expr.span,
      });
    }

    return UNKNOWN;
  }

  private inferIndexExpr(expr: ast.IndexExpr): Type {
    const arrType = this.inferExpr(expr.expr);
    const indexType = this.inferExpr(expr.index);

    // Index must be integer
    if (!isInteger(indexType) && indexType.kind !== "TypeVar") {
      this.error({
        message: `Array index must be integer, got ${typeToString(indexType)}`,
        span: expr.index.span,
      });
    }

    if (arrType.kind === "Array") {
      return arrType.element;
    }

    if (arrType.kind !== "TypeVar" && arrType.kind !== "Unknown") {
      this.error({
        message: `Cannot index into type ${typeToString(arrType)}`,
        span: expr.span,
      });
    }

    return UNKNOWN;
  }

  private inferTupleExpr(expr: ast.TupleExpr): Type {
    // Empty tuple is unit
    if (expr.elements.length === 0) {
      return UNIT;
    }

    const elementTypes = expr.elements.map((e) => this.inferExpr(e));
    return tupleType(elementTypes);
  }

  private inferArrayExpr(expr: ast.ArrayExpr): Type {
    if (expr.elements.length === 0) {
      // Empty array - need type annotation or context
      return arrayType(freshTypeVar());
    }

    const firstType = this.inferExpr(expr.elements[0]);

    // All elements must have same type
    for (let i = 1; i < expr.elements.length; i++) {
      const elemType = this.inferExpr(expr.elements[i]);
      this.addConstraint(elemType, firstType, `Array element ${i + 1} type mismatch`, expr.elements[i].span);
    }

    return arrayType(firstType);
  }

  private inferStructExpr(expr: ast.StructExpr): Type {
    const typeDef = this.env.lookupType(expr.name);
    if (!typeDef) {
      this.error({
        message: `Unknown struct: ${expr.name}`,
        span: expr.span,
      });
      return UNKNOWN;
    }

    if (typeDef.type.kind !== "Struct") {
      this.error({
        message: `'${expr.name}' is not a struct`,
        span: expr.span,
      });
      return UNKNOWN;
    }

    const structType = typeDef.type;

    // Check field types
    const providedFields = new Set<string>();
    for (const field of expr.fields) {
      providedFields.add(field.name);

      const expectedType = structType.fields.get(field.name);
      if (!expectedType) {
        this.error({
          message: `Struct '${expr.name}' has no field '${field.name}'`,
          span: expr.span, // TODO: better span
        });
        continue;
      }

      const valueType = this.inferExpr(field.value);
      this.addConstraint(valueType, expectedType, `Field '${field.name}' type mismatch`, expr.span);
    }

    // Check for missing fields
    for (const fieldName of structType.fields.keys()) {
      if (!providedFields.has(fieldName) && !expr.spread) {
        this.error({
          message: `Missing field '${fieldName}' in struct '${expr.name}'`,
          span: expr.span,
        });
      }
    }

    return structType;
  }

  private inferIfExpr(expr: ast.IfExpr): Type {
    const condType = this.inferExpr(expr.condition);
    this.addConstraint(condType, BOOL, "If condition must be bool", expr.condition.span);

    const thenType = this.inferExpr(expr.then);

    if (expr.else_) {
      const elseType = this.inferExpr(expr.else_);
      this.addConstraint(thenType, elseType, "If branches must have same type", expr.span);
      return thenType;
    }

    // No else branch - must be unit
    this.addConstraint(thenType, UNIT, "If without else must have unit type", expr.span);
    return UNIT;
  }

  private inferMatchExpr(expr: ast.MatchExpr): Type {
    const scrutineeType = this.inferExpr(expr.expr);

    if (expr.arms.length === 0) {
      this.error({
        message: "Match expression must have at least one arm",
        span: expr.span,
      });
      return UNKNOWN;
    }

    let resultType: Type | null = null;

    for (const arm of expr.arms) {
      this.env.pushScope();

      // Bind pattern variables
      this.bindPattern(arm.pattern, scrutineeType, false);

      // Check guard if present
      if (arm.guard) {
        const guardType = this.inferExpr(arm.guard);
        this.addConstraint(guardType, BOOL, "Match guard must be bool", arm.guard.span);
      }

      // Infer arm body
      const armType = this.inferExpr(arm.body);

      if (resultType === null) {
        resultType = armType;
      } else {
        this.addConstraint(armType, resultType, "Match arms must have same type", arm.body.span);
      }

      this.env.popScope();
    }

    return resultType ?? UNKNOWN;
  }

  private inferBlockExpr(expr: ast.BlockExpr): Type {
    this.env.pushScope();

    for (const stmt of expr.stmts) {
      this.checkStmt(stmt);
    }

    const resultType = expr.expr ? this.inferExpr(expr.expr) : UNIT;

    this.env.popScope();
    return resultType;
  }

  private inferClosureExpr(expr: ast.ClosureExpr): Type {
    this.env.pushScope();

    const paramTypes: Type[] = [];
    for (const param of expr.params) {
      const paramType = param.type ? this.resolveAstType(param.type, []) : freshTypeVar();
      paramTypes.push(paramType);
      this.bindPattern(param.pattern, paramType, false);
    }

    const bodyType = this.inferExpr(expr.body);
    const retType = expr.ret ? this.resolveAstType(expr.ret, []) : bodyType;

    if (expr.ret) {
      this.addConstraint(bodyType, retType, "Closure body type mismatch", expr.body.span);
    }

    this.env.popScope();

    return functionType(paramTypes, retType);
  }

  private inferReturnExpr(expr: ast.ReturnExpr): Type {
    const valueType = expr.value ? this.inferExpr(expr.value) : UNIT;

    if (this.currentReturnType) {
      this.addConstraint(valueType, this.currentReturnType, "Return type mismatch", expr.span);
    }

    return NEVER;
  }

  private inferLoopExpr(expr: ast.LoopExpr): Type {
    this.env.pushScope();
    this.inferExpr(expr.body);
    this.env.popScope();

    // Loop returns Never unless broken with a value
    // For now, return unit
    return UNIT;
  }

  private inferWhileExpr(expr: ast.WhileExpr): Type {
    const condType = this.inferExpr(expr.condition);
    this.addConstraint(condType, BOOL, "While condition must be bool", expr.condition.span);

    this.env.pushScope();
    this.inferExpr(expr.body);
    this.env.popScope();

    return UNIT;
  }

  private inferForExpr(expr: ast.ForExpr): Type {
    const iterType = this.inferExpr(expr.iter);
    // TODO: Check that iterType is iterable and extract element type
    const elemType = freshTypeVar();

    this.env.pushScope();
    this.bindPattern(expr.pattern, elemType, false);
    this.inferExpr(expr.body);
    this.env.popScope();

    return UNIT;
  }

  private inferRangeExpr(expr: ast.RangeExpr): Type {
    let rangeType: Type = I32;

    if (expr.start) {
      rangeType = this.inferExpr(expr.start);
      if (!isInteger(rangeType) && rangeType.kind !== "TypeVar") {
        this.error({
          message: `Range bounds must be integer, got ${typeToString(rangeType)}`,
          span: expr.start.span,
        });
      }
    }

    if (expr.end) {
      const endType = this.inferExpr(expr.end);
      this.addConstraint(endType, rangeType, "Range bounds must have same type", expr.end.span);
    }

    // Return Range<T> type - for now just return a placeholder
    return structType("Range", new Map([["start", rangeType], ["end", rangeType]]));
  }

  private inferAssignExpr(expr: ast.AssignExpr): Type {
    const targetType = this.inferExpr(expr.target);
    const valueType = this.inferExpr(expr.value);

    // Check mutability
    if (expr.target.kind === "Identifier") {
      const symbol = this.env.lookup(expr.target.name);
      if (symbol && !symbol.mutable) {
        this.error({
          message: `Cannot assign to immutable variable '${expr.target.name}'`,
          span: expr.span,
        });
      }
    }

    if (expr.op) {
      // Compound assignment (+=, -= etc.)
      // Both sides must be compatible with the operator
      this.addConstraint(targetType, valueType, "Assignment type mismatch", expr.span);
    } else {
      this.addConstraint(valueType, targetType, "Assignment type mismatch", expr.span);
    }

    return UNIT;
  }

  private inferTryExpr(expr: ast.TryExpr): Type {
    const innerType = this.inferExpr(expr.expr);
    // TODO: Properly handle Result/Option types
    return innerType;
  }

  private inferAwaitExpr(expr: ast.AwaitExpr): Type {
    const futureType = this.inferExpr(expr.expr);
    // TODO: Properly handle Future types
    return futureType;
  }

  private inferPathExpr(expr: ast.PathExpr): Type {
    // Handle enum variant paths like Option::None
    if (expr.segments.length === 2) {
      const [enumName, variantName] = expr.segments;
      const typeDef = this.env.lookupType(enumName);
      if (typeDef && typeDef.type.kind === "Enum") {
        const variant = typeDef.type.variants.get(variantName);
        if (variant === undefined) {
          this.error({
            message: `Enum '${enumName}' has no variant '${variantName}'`,
            span: expr.span,
          });
          return UNKNOWN;
        }

        // Unit variant - return the enum type
        if (variant === null) {
          return typeDef.type;
        }

        // Tuple variant - return a constructor function
        return functionType(variant, typeDef.type);
      }
    }

    // Try as a qualified name
    const fullName = expr.segments.join("::");
    const symbol = this.env.lookup(fullName);
    if (symbol) {
      return symbol.type;
    }

    this.error({
      message: `Unknown path: ${fullName}`,
      span: expr.span,
    });
    return UNKNOWN;
  }

  // ---------------------------------------------------------------------------
  // STATEMENT CHECKING
  // ---------------------------------------------------------------------------

  private checkStmt(stmt: ast.Stmt): void {
    switch (stmt.kind) {
      case "LetStmt":
        this.checkLetStmt(stmt);
        break;
      case "ExprStmt":
        this.inferExpr(stmt.expr);
        break;
      default:
        // Items in statement position
        this.checkItem(stmt);
    }
  }

  private checkLetStmt(stmt: ast.LetStmt): void {
    let type: Type;

    if (stmt.init) {
      const initType = this.inferExpr(stmt.init);

      if (stmt.type) {
        const annotatedType = this.resolveAstType(stmt.type, []);
        this.addConstraint(initType, annotatedType, "Let binding type mismatch", stmt.span);
        type = annotatedType;
      } else {
        type = initType;
      }
    } else if (stmt.type) {
      type = this.resolveAstType(stmt.type, []);
    } else {
      this.error({
        message: "Let binding must have type annotation or initializer",
        span: stmt.span,
      });
      type = UNKNOWN;
    }

    this.bindPattern(stmt.pattern, type, stmt.mutable);
  }

  // ---------------------------------------------------------------------------
  // PATTERN BINDING
  // ---------------------------------------------------------------------------

  private bindPattern(pattern: ast.Pattern, type: Type, mutable: boolean): void {
    switch (pattern.kind) {
      case "WildcardPattern":
        // Nothing to bind
        break;

      case "IdentPattern":
        this.env.define(pattern.name, type, pattern.mutable || mutable, "variable");
        break;

      case "TuplePattern":
        if (type.kind === "Tuple") {
          if (pattern.elements.length !== type.elements.length) {
            this.error({
              message: `Tuple pattern has ${pattern.elements.length} elements, expected ${type.elements.length}`,
              span: pattern.span,
            });
          }
          for (let i = 0; i < pattern.elements.length && i < type.elements.length; i++) {
            this.bindPattern(pattern.elements[i], type.elements[i], mutable);
          }
        } else if (type.kind !== "TypeVar" && type.kind !== "Unknown") {
          this.error({
            message: `Cannot destructure ${typeToString(type)} as tuple`,
            span: pattern.span,
          });
        }
        break;

      case "StructPattern":
        if (type.kind === "Struct") {
          for (const field of pattern.fields) {
            const fieldType = type.fields.get(field.name);
            if (!fieldType) {
              this.error({
                message: `Struct '${type.name}' has no field '${field.name}'`,
                span: pattern.span,
              });
              continue;
            }
            this.bindPattern(field.pattern, fieldType, mutable);
          }
        } else if (type.kind !== "TypeVar" && type.kind !== "Unknown") {
          this.error({
            message: `Cannot destructure ${typeToString(type)} as struct`,
            span: pattern.span,
          });
        }
        break;

      case "EnumPattern":
        // TODO: Enum pattern binding
        break;

      case "LiteralPattern":
        // Type check the literal matches
        const litType = this.inferExpr(pattern.value);
        this.addConstraint(litType, type, "Pattern type mismatch", pattern.span);
        break;

      case "OrPattern":
        for (const sub of pattern.patterns) {
          this.bindPattern(sub, type, mutable);
        }
        break;

      case "RangePattern":
        // Type check range bounds
        if (pattern.start) {
          const startType = this.inferExpr(pattern.start);
          this.addConstraint(startType, type, "Range pattern type mismatch", pattern.span);
        }
        if (pattern.end) {
          const endType = this.inferExpr(pattern.end);
          this.addConstraint(endType, type, "Range pattern type mismatch", pattern.span);
        }
        break;
    }
  }

  // ---------------------------------------------------------------------------
  // TYPE RESOLUTION
  // ---------------------------------------------------------------------------

  private resolveAstType(astType: ast.Type, typeParams: TypeVariable[]): Type {
    switch (astType.kind) {
      case "NamedType":
        return this.resolveNamedType(astType.name, typeParams);

      case "GenericType": {
        const base = this.resolveNamedType(astType.name, typeParams);
        const args = astType.args.map((a) => this.resolveAstType(a, typeParams));
        return { kind: "Generic", base, args };
      }

      case "TupleType": {
        if (astType.elements.length === 0) {
          return UNIT;
        }
        return tupleType(astType.elements.map((e) => this.resolveAstType(e, typeParams)));
      }

      case "ArrayType":
        return arrayType(this.resolveAstType(astType.element, typeParams));

      case "FunctionType":
        return functionType(
          astType.params.map((p) => this.resolveAstType(p, typeParams)),
          this.resolveAstType(astType.ret, typeParams),
        );

      case "RefType":
        // For now, treat refs as the underlying type
        return this.resolveAstType(astType.inner, typeParams);

      case "InferredType":
        return freshTypeVar();

      default:
        return UNKNOWN;
    }
  }

  private resolveNamedType(name: string, typeParams: TypeVariable[]): Type {
    // Check type parameters first
    for (const tv of typeParams) {
      if (tv.name === name) {
        return tv;
      }
    }

    // Check primitives
    const primitiveNames: PrimitiveName[] = [
      "i8", "i16", "i32", "i64",
      "u8", "u16", "u32", "u64",
      "f32", "f64",
      "bool", "char", "str", "unit",
    ];

    if (primitiveNames.includes(name as PrimitiveName)) {
      return primitive(name as PrimitiveName);
    }

    // Check user-defined types
    const typeDef = this.env.lookupType(name);
    if (typeDef) {
      return typeDef.type;
    }

    this.error({
      message: `Unknown type: ${name}`,
      span: { start: { line: 0, column: 0, offset: 0 }, end: { line: 0, column: 0, offset: 0 } },
    });
    return UNKNOWN;
  }

  // ---------------------------------------------------------------------------
  // HELPERS
  // ---------------------------------------------------------------------------

  private addConstraint(t1: Type, t2: Type, message: string, span: Span): void {
    // Try immediate unification to catch errors early
    try {
      unify(t1, t2);
    } catch (e) {
      if (e instanceof UnificationError) {
        this.error({ message: `${message}: ${e.message}`, span });
        return;
      }
    }
    // Also add to constraint set for full solving
    this.constraints.add(t1, t2, message);
  }

  private error(err: TypeError): void {
    this.errors.push(err);
  }
}

// =============================================================================
// CONVENIENCE FUNCTION
// =============================================================================

export function typeCheck(module: ast.Module): { errors: TypeError[]; types: Map<ast.Node, Type> } {
  const checker = new TypeChecker();
  return checker.check(module);
}
