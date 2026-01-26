/**
 * Lingua Code Generator
 *
 * Compiles type-checked Lingua AST to WebAssembly.
 */

import type * as ast from "../parser/ast.ts";
import type { Type } from "../types/types.ts";
import { WasmModule, CodeBuilder, ValType, BLOCK_TYPE_VOID } from "./wasm.ts";

// =============================================================================
// COMPILATION CONTEXT
// =============================================================================

interface LocalVar {
  index: number;
  type: ValType;
  structName?: string;  // If this is a struct pointer, the struct name
  tupleLayout?: TupleLayout;  // If this is a tuple pointer, the layout
}

interface TupleLayout {
  size: number;
  elements: { offset: number; type: ValType }[];
}

interface FunctionContext {
  builder: CodeBuilder;
  locals: Map<string, LocalVar>;
  paramCount: number;
  returnType: ValType | null;
  loopLabels: number[];  // Stack of loop label depths for break/continue
  blockDepth: number;
}

interface CompiledFunction {
  name: string;
  typeIdx: number;
  funcIdx: number;
  paramTypes: ValType[];
  returnType: ValType | null;
}

interface FieldLayout {
  offset: number;
  size: number;
  type: ValType;
}

interface StructLayout {
  size: number;
  fields: Map<string, FieldLayout>;
}

interface EnumVariantLayout {
  tag: number;
  fields?: FieldLayout[];  // For tuple variants
}

interface EnumLayout {
  name: string;
  variants: Map<string, EnumVariantLayout>;
  isSimple: boolean;  // True if all variants are unit (no data)
  size: number;       // Total size if has data variants
}

// =============================================================================
// CODE GENERATOR
// =============================================================================

export class CodeGenerator {
  private module: WasmModule;
  private functions: Map<string, CompiledFunction> = new Map();
  private structs: Map<string, StructLayout> = new Map();
  private enums: Map<string, EnumLayout> = new Map();
  private currentFunc: FunctionContext | null = null;
  private heapPtrGlobal: number = -1;
  private hasMemory: boolean = false;
  private hasBuiltins: boolean = false;

  constructor() {
    this.module = new WasmModule();
  }

  // Register built-in functions (imports from host environment)
  private registerBuiltins(): void {
    if (this.hasBuiltins) return;
    this.hasBuiltins = true;

    // print_i32: prints an i32 value
    const printI32Type = this.module.getOrAddType([ValType.I32], []);
    const printI32Idx = this.module.addFuncImport("env", "print_i32", printI32Type);
    this.functions.set("print", {
      name: "print",
      typeIdx: printI32Type,
      funcIdx: printI32Idx,
      paramTypes: [ValType.I32],
      returnType: null,
    });

    // print_i32_ln: prints an i32 value with newline (alias)
    this.functions.set("println", {
      name: "println",
      typeIdx: printI32Type,
      funcIdx: printI32Idx,
      paramTypes: [ValType.I32],
      returnType: null,
    });
  }

  // Initialize memory and heap pointer for struct allocation
  private initMemory(): void {
    if (this.hasMemory) return;
    this.hasMemory = true;

    // Add 1 page of memory (64KB)
    this.module.addMemory(1);

    // Add heap pointer global, starts at 1024 to leave room for stack
    // Global init expression: i32.const 1024, end
    this.heapPtrGlobal = this.module.addGlobal(ValType.I32, true, [0x41, 0x80, 0x08]);
  }

  // Register a struct layout
  private registerStruct(structItem: ast.StructItem): void {
    let offset = 0;
    const fields = new Map<string, FieldLayout>();

    for (const field of structItem.fields) {
      const valType = this.astTypeToValType(field.type);
      const size = this.valTypeSize(valType);

      // Align offset to field size
      const alignment = size;
      offset = Math.ceil(offset / alignment) * alignment;

      fields.set(field.name, {
        offset,
        size,
        type: valType,
      });

      offset += size;
    }

    // Align total size to 4 bytes
    const totalSize = Math.ceil(offset / 4) * 4;

    this.structs.set(structItem.name, {
      size: totalSize || 4, // Minimum 4 bytes for empty struct
      fields,
    });
  }

  private valTypeSize(type: ValType): number {
    switch (type) {
      case ValType.I32:
      case ValType.F32:
        return 4;
      case ValType.I64:
      case ValType.F64:
        return 8;
      default:
        return 4;
    }
  }

  // Register an enum layout
  private registerEnum(enumItem: ast.EnumItem): void {
    const variants = new Map<string, EnumVariantLayout>();
    let isSimple = true;
    let maxDataSize = 0;

    for (let i = 0; i < enumItem.variants.length; i++) {
      const variant = enumItem.variants[i];

      if (variant.fields && variant.fields.length > 0) {
        // Tuple variant with data
        isSimple = false;
        const fields: FieldLayout[] = [];
        let offset = 4; // First 4 bytes for tag

        for (const fieldType of variant.fields) {
          const valType = this.astTypeToValType(fieldType);
          const size = this.valTypeSize(valType);
          const alignment = size;
          offset = Math.ceil(offset / alignment) * alignment;

          fields.push({ offset, size, type: valType });
          offset += size;
        }

        maxDataSize = Math.max(maxDataSize, offset);
        variants.set(variant.name, { tag: i, fields });
      } else {
        // Unit variant (no data)
        variants.set(variant.name, { tag: i });
      }
    }

    // Total size: for simple enums just 4 bytes (tag), otherwise tag + max data
    const size = isSimple ? 4 : Math.ceil(maxDataSize / 4) * 4;

    this.enums.set(enumItem.name, {
      name: enumItem.name,
      variants,
      isSimple,
      size,
    });
  }

  // ---------------------------------------------------------------------------
  // PUBLIC API
  // ---------------------------------------------------------------------------

  compile(program: ast.Module): Uint8Array {
    // Register built-in functions (imports)
    this.registerBuiltins();

    // First pass: register all struct and enum layouts
    for (const item of program.items) {
      if (item.kind === "StructItem") {
        this.registerStruct(item);
      } else if (item.kind === "EnumItem") {
        this.registerEnum(item);
      }
    }

    // Initialize memory if we have structs or non-simple enums
    const hasDataEnums = [...this.enums.values()].some((e) => !e.isSimple);
    if (this.structs.size > 0 || hasDataEnums) {
      this.initMemory();
    }

    // Second pass: register all function signatures
    for (const item of program.items) {
      if (item.kind === "FnItem") {
        this.registerFunction(item);
      }
    }

    // Third pass: compile function bodies
    for (const item of program.items) {
      if (item.kind === "FnItem" && item.body) {
        this.compileFunction(item);
      }
    }

    // Export public functions
    for (const item of program.items) {
      if (item.kind === "FnItem" && item.vis === "public") {
        const func = this.functions.get(item.name);
        if (func) {
          this.module.exportFunc(item.name, func.funcIdx);
        }
      }
    }

    return this.module.emit();
  }

  // ---------------------------------------------------------------------------
  // FUNCTION COMPILATION
  // ---------------------------------------------------------------------------

  private registerFunction(fn: ast.FnItem): void {
    const paramTypes = fn.params.map((p) => this.astTypeToValType(p.type));
    const returnType = fn.ret ? this.astTypeToValType(fn.ret) : null;

    const results = returnType !== null ? [returnType] : [];
    const typeIdx = this.module.getOrAddType(paramTypes, results);
    const funcIdx = this.module.addFunction(typeIdx);

    this.functions.set(fn.name, {
      name: fn.name,
      typeIdx,
      funcIdx,
      paramTypes,
      returnType,
    });
  }

  private compileFunction(fn: ast.FnItem): void {
    const compiled = this.functions.get(fn.name)!;
    const builder = new CodeBuilder();

    // Set up context
    this.currentFunc = {
      builder,
      locals: new Map(),
      paramCount: fn.params.length,
      returnType: compiled.returnType,
      loopLabels: [],
      blockDepth: 0,
    };

    // Register parameters as locals (params come first in local index space)
    for (let i = 0; i < fn.params.length; i++) {
      const param = fn.params[i];
      const paramName = this.getPatternName(param.pattern);
      if (paramName) {
        // Check if this is a struct type
        let structName: string | undefined;
        if (param.type.kind === "NamedType" && this.structs.has(param.type.name)) {
          structName = param.type.name;
        }
        this.currentFunc.locals.set(paramName, {
          index: i,
          type: compiled.paramTypes[i],
          structName,
        });
      }
    }

    // Compile body
    if (fn.body) {
      this.compileBlockExpr(fn.body, compiled.returnType !== null);
    }

    // Add code to module
    this.module.addCode(
      compiled.typeIdx,
      builder.getLocals(),
      builder.getCode(),
    );

    this.currentFunc = null;
  }

  // ---------------------------------------------------------------------------
  // EXPRESSION COMPILATION
  // ---------------------------------------------------------------------------

  private compileExpr(expr: ast.Expr, needsValue: boolean = true): void {
    const builder = this.currentFunc!.builder;

    switch (expr.kind) {
      case "IntLiteral":
        if (needsValue) {
          // Default to i32 for now
          builder.i32Const(Number(expr.value));
        }
        break;

      case "FloatLiteral":
        if (needsValue) {
          builder.f64Const(expr.value);
        }
        break;

      case "BoolLiteral":
        if (needsValue) {
          builder.i32Const(expr.value ? 1 : 0);
        }
        break;

      case "Identifier":
        if (needsValue) {
          const local = this.currentFunc!.locals.get(expr.name);
          if (local) {
            builder.localGet(local.index);
          } else {
            throw new Error(`Unknown variable: ${expr.name}`);
          }
        }
        break;

      case "BinaryExpr":
        this.compileBinaryExpr(expr, needsValue);
        break;

      case "UnaryExpr":
        this.compileUnaryExpr(expr, needsValue);
        break;

      case "CallExpr":
        this.compileCallExpr(expr, needsValue);
        break;

      case "IfExpr":
        this.compileIfExpr(expr, needsValue);
        break;

      case "BlockExpr":
        this.compileBlockExpr(expr, needsValue);
        break;

      case "WhileExpr":
        this.compileWhileExpr(expr);
        if (needsValue) {
          // While loops return unit, but if we need a value, push a dummy
        }
        break;

      case "LoopExpr":
        this.compileLoopExpr(expr, needsValue);
        break;

      case "ReturnExpr":
        this.compileReturnExpr(expr);
        break;

      case "BreakExpr":
        this.compileBreakExpr(expr);
        break;

      case "ContinueExpr":
        this.compileContinueExpr(expr);
        break;

      case "AssignExpr":
        this.compileAssignExpr(expr);
        break;

      case "TupleExpr":
        this.compileTupleExpr(expr, needsValue);
        break;

      case "StructExpr":
        this.compileStructExpr(expr, needsValue);
        break;

      case "FieldExpr":
        this.compileFieldExpr(expr, needsValue);
        break;

      case "PathExpr":
        this.compilePathExpr(expr, needsValue);
        break;

      case "MatchExpr":
        this.compileMatchExpr(expr, needsValue);
        break;

      case "ArrayExpr":
        this.compileArrayExpr(expr, needsValue);
        break;

      case "IndexExpr":
        this.compileIndexExpr(expr, needsValue);
        break;

      default:
        throw new Error(`Unsupported expression: ${expr.kind}`);
    }
  }

  private compileBinaryExpr(expr: ast.BinaryExpr, needsValue: boolean): void {
    const builder = this.currentFunc!.builder;

    // Short-circuit evaluation for && and ||
    if (expr.op === "&&") {
      this.compileExpr(expr.left, true);
      builder.if_(ValType.I32);
      this.compileExpr(expr.right, true);
      builder.else_();
      builder.i32Const(0);
      builder.end();
      if (!needsValue) builder.drop();
      return;
    }

    if (expr.op === "||") {
      this.compileExpr(expr.left, true);
      builder.if_(ValType.I32);
      builder.i32Const(1);
      builder.else_();
      this.compileExpr(expr.right, true);
      builder.end();
      if (!needsValue) builder.drop();
      return;
    }

    // Normal binary operations
    this.compileExpr(expr.left, true);
    this.compileExpr(expr.right, true);

    // TODO: Detect types properly. For now, assume i32 for most ops.
    switch (expr.op) {
      case "+": builder.i32Add(); break;
      case "-": builder.i32Sub(); break;
      case "*": builder.i32Mul(); break;
      case "/": builder.i32DivS(); break;
      case "%": builder.i32RemS(); break;
      case "==": builder.i32Eq(); break;
      case "!=": builder.i32Ne(); break;
      case "<": builder.i32LtS(); break;
      case ">": builder.i32GtS(); break;
      case "<=": builder.i32LeS(); break;
      case ">=": builder.i32GeS(); break;
      case "&": builder.i32And(); break;
      case "|": builder.i32Or(); break;
      case "^": builder.i32Xor(); break;
      case "<<": builder.i32Shl(); break;
      case ">>": builder.i32ShrS(); break;
      default:
        throw new Error(`Unsupported binary operator: ${expr.op}`);
    }

    if (!needsValue) builder.drop();
  }

  private compileUnaryExpr(expr: ast.UnaryExpr, needsValue: boolean): void {
    const builder = this.currentFunc!.builder;

    switch (expr.op) {
      case "-":
        // Negate: 0 - value
        builder.i32Const(0);
        this.compileExpr(expr.expr, true);
        builder.i32Sub();
        break;
      case "!":
        // Logical not: value == 0
        this.compileExpr(expr.expr, true);
        builder.i32Eqz();
        break;
      default:
        throw new Error(`Unsupported unary operator: ${expr.op}`);
    }

    if (!needsValue) builder.drop();
  }

  private compileCallExpr(expr: ast.CallExpr, needsValue: boolean): void {
    const builder = this.currentFunc!.builder;

    // Check if this is an enum variant constructor (PathExpr callee)
    if (expr.callee.kind === "PathExpr" && expr.callee.segments.length === 2) {
      const [enumName, variantName] = expr.callee.segments;
      const enumLayout = this.enums.get(enumName);

      if (enumLayout) {
        const variant = enumLayout.variants.get(variantName);
        if (!variant) {
          throw new Error(`Unknown enum variant: ${enumName}::${variantName}`);
        }

        if (!variant.fields || variant.fields.length === 0) {
          // Unit variant being called - just push the tag
          if (needsValue) {
            builder.i32Const(variant.tag);
          }
          return;
        }

        // Tuple variant - allocate memory, store tag and fields
        this.initMemory();

        // Allocate memory
        builder.globalGet(this.heapPtrGlobal);
        const ptrLocal = this.currentFunc!.paramCount + builder.addLocal(ValType.I32);
        builder.localTee(ptrLocal);

        // Bump heap pointer
        builder.i32Const(enumLayout.size);
        builder.i32Add();
        builder.globalSet(this.heapPtrGlobal);

        // Store tag at offset 0
        builder.localGet(ptrLocal);
        builder.i32Const(variant.tag);
        builder.i32Store(2, 0);

        // Store each field
        for (let i = 0; i < expr.args.length && i < variant.fields.length; i++) {
          builder.localGet(ptrLocal);
          this.compileExpr(expr.args[i], true);
          builder.i32Store(2, variant.fields[i].offset);
        }

        // Leave pointer on stack if needed
        if (needsValue) {
          builder.localGet(ptrLocal);
        }
        return;
      }
    }

    // Regular function call
    if (expr.callee.kind !== "Identifier") {
      throw new Error("Only direct function calls supported");
    }

    const funcName = expr.callee.name;
    const func = this.functions.get(funcName);
    if (!func) {
      throw new Error(`Unknown function: ${funcName}`);
    }

    // Compile arguments
    for (const arg of expr.args) {
      this.compileExpr(arg, true);
    }

    // Call function
    builder.call(func.funcIdx);

    // Drop result if not needed
    if (!needsValue && func.returnType !== null) {
      builder.drop();
    }
  }

  private compileIfExpr(expr: ast.IfExpr, needsValue: boolean): void {
    const builder = this.currentFunc!.builder;

    // Compile condition
    this.compileExpr(expr.condition, true);

    // Determine block type
    const blockType = needsValue ? ValType.I32 : BLOCK_TYPE_VOID;

    builder.if_(blockType);
    this.currentFunc!.blockDepth++;

    // Compile then branch
    this.compileExpr(expr.then, needsValue);

    if (expr.else_) {
      builder.else_();
      this.compileExpr(expr.else_, needsValue);
    } else if (needsValue) {
      // If we need a value but have no else, this is an error
      // (should be caught by type checker)
      builder.else_();
      builder.i32Const(0); // dummy value
    }

    builder.end();
    this.currentFunc!.blockDepth--;
  }

  private compileBlockExpr(expr: ast.BlockExpr, needsValue: boolean): void {
    // Compile statements
    for (const stmt of expr.stmts) {
      this.compileStmt(stmt);
    }

    // Compile trailing expression if any
    if (expr.expr) {
      this.compileExpr(expr.expr, needsValue);
    }
  }

  private compileWhileExpr(expr: ast.WhileExpr): void {
    const builder = this.currentFunc!.builder;

    // Structure:
    // block $break
    //   loop $continue
    //     br_if $break (if condition is false)
    //     body
    //     br $continue
    //   end
    // end

    builder.block(BLOCK_TYPE_VOID);  // $break
    this.currentFunc!.blockDepth++;

    builder.loop(BLOCK_TYPE_VOID);   // $continue
    this.currentFunc!.blockDepth++;
    this.currentFunc!.loopLabels.push(this.currentFunc!.blockDepth);

    // Compile condition and branch out if false
    this.compileExpr(expr.condition, true);
    builder.i32Eqz();
    builder.brIf(1);  // break out of both loop and block

    // Compile body
    this.compileExpr(expr.body, false);

    // Continue loop
    builder.br(0);

    builder.end();  // end loop
    this.currentFunc!.blockDepth--;
    this.currentFunc!.loopLabels.pop();

    builder.end();  // end block
    this.currentFunc!.blockDepth--;
  }

  private compileLoopExpr(expr: ast.LoopExpr, _needsValue: boolean): void {
    const builder = this.currentFunc!.builder;

    // Infinite loop structure:
    // block $break
    //   loop $continue
    //     body
    //     br $continue
    //   end
    // end

    builder.block(BLOCK_TYPE_VOID);
    this.currentFunc!.blockDepth++;

    builder.loop(BLOCK_TYPE_VOID);
    this.currentFunc!.blockDepth++;
    this.currentFunc!.loopLabels.push(this.currentFunc!.blockDepth);

    // Compile body
    this.compileExpr(expr.body, false);

    // Continue loop
    builder.br(0);

    builder.end();
    this.currentFunc!.blockDepth--;
    this.currentFunc!.loopLabels.pop();

    builder.end();
    this.currentFunc!.blockDepth--;
  }

  private compileReturnExpr(expr: ast.ReturnExpr): void {
    const builder = this.currentFunc!.builder;

    if (expr.value) {
      this.compileExpr(expr.value, true);
    }

    builder.return_();
  }

  private compileBreakExpr(_expr: ast.BreakExpr): void {
    const builder = this.currentFunc!.builder;
    // Break out of the enclosing block (which is 1 level up from the loop)
    builder.br(1);
  }

  private compileContinueExpr(_expr: ast.ContinueExpr): void {
    const builder = this.currentFunc!.builder;
    // Continue to the loop header (which is at depth 0 relative to loop)
    builder.br(0);
  }

  private compileAssignExpr(expr: ast.AssignExpr): void {
    const builder = this.currentFunc!.builder;

    if (expr.target.kind !== "Identifier") {
      throw new Error("Only simple variable assignment supported");
    }

    const varName = expr.target.name;
    const local = this.currentFunc!.locals.get(varName);
    if (!local) {
      throw new Error(`Unknown variable: ${varName}`);
    }

    if (expr.op) {
      // Compound assignment: x += y -> x = x + y
      builder.localGet(local.index);
      this.compileExpr(expr.value, true);

      switch (expr.op) {
        case "+": builder.i32Add(); break;
        case "-": builder.i32Sub(); break;
        case "*": builder.i32Mul(); break;
        case "/": builder.i32DivS(); break;
        case "%": builder.i32RemS(); break;
        default:
          throw new Error(`Unsupported compound assignment operator: ${expr.op}`);
      }
    } else {
      this.compileExpr(expr.value, true);
    }

    builder.localSet(local.index);
  }

  private compileTupleExpr(expr: ast.TupleExpr, needsValue: boolean): void {
    const builder = this.currentFunc!.builder;

    if (expr.elements.length === 0) {
      // Unit - no value needed
      return;
    }

    // Initialize memory if needed
    this.initMemory();

    // Calculate tuple layout
    const layout = this.calculateTupleLayout(expr.elements);

    // Allocate memory for tuple (bump allocator)
    builder.globalGet(this.heapPtrGlobal);

    // Save pointer to a local for element writes
    const ptrLocal = this.currentFunc!.paramCount + builder.addLocal(ValType.I32);
    builder.localTee(ptrLocal);

    // Bump heap pointer by tuple size
    builder.i32Const(layout.size);
    builder.i32Add();
    builder.globalSet(this.heapPtrGlobal);

    // Write each element
    for (let i = 0; i < expr.elements.length; i++) {
      const elem = expr.elements[i];
      const elemLayout = layout.elements[i];

      // Get base pointer
      builder.localGet(ptrLocal);

      // Compile element value
      this.compileExpr(elem, true);

      // Store at offset
      builder.i32Store(2, elemLayout.offset);
    }

    // Leave pointer on stack if value is needed
    if (needsValue) {
      builder.localGet(ptrLocal);
    }

    // Store the layout for later field access
    // This is a workaround - ideally we'd track this through the type system
    this.lastTupleLayout = layout;
  }

  private lastTupleLayout: TupleLayout | null = null;

  private calculateTupleLayout(elements: ast.Expr[]): TupleLayout {
    let offset = 0;
    const elemLayouts: { offset: number; type: ValType }[] = [];

    for (const elem of elements) {
      // For now, assume all elements are i32
      const valType = ValType.I32;
      const size = 4;
      const alignment = 4;

      offset = Math.ceil(offset / alignment) * alignment;
      elemLayouts.push({ offset, type: valType });
      offset += size;
    }

    // Align total size to 4 bytes
    const totalSize = Math.ceil(offset / 4) * 4;

    return {
      size: totalSize || 4,
      elements: elemLayouts,
    };
  }

  private compileStructExpr(expr: ast.StructExpr, needsValue: boolean): void {
    const builder = this.currentFunc!.builder;

    // Get struct name
    const structName = expr.name;
    const layout = this.structs.get(structName);
    if (!layout) {
      throw new Error(`Unknown struct: ${structName}`);
    }

    // Allocate memory for struct (bump allocator)
    // Get current heap pointer
    builder.globalGet(this.heapPtrGlobal);

    // Save pointer to a local for field writes
    const ptrLocal = this.currentFunc!.paramCount + builder.addLocal(ValType.I32);
    builder.localTee(ptrLocal);

    // Bump heap pointer by struct size
    builder.i32Const(layout.size);
    builder.i32Add();
    builder.globalSet(this.heapPtrGlobal);

    // Write each field
    for (const fieldInit of expr.fields) {
      const fieldLayout = layout.fields.get(fieldInit.name);
      if (!fieldLayout) {
        throw new Error(`Unknown field: ${fieldInit.name}`);
      }

      // Get base pointer
      builder.localGet(ptrLocal);

      // Compile field value
      this.compileExpr(fieldInit.value, true);

      // Store at offset
      builder.i32Store(2, fieldLayout.offset);
    }

    // Leave pointer on stack if value is needed
    if (needsValue) {
      builder.localGet(ptrLocal);
    }
  }

  private compileFieldExpr(expr: ast.FieldExpr, needsValue: boolean): void {
    const builder = this.currentFunc!.builder;

    // Check if this is a tuple field access (numeric index)
    const fieldIndex = parseInt(expr.field, 10);
    const isTupleAccess = !isNaN(fieldIndex);

    // Compile the expression to get its pointer
    this.compileExpr(expr.expr, true);

    if (!needsValue) {
      builder.drop();
      return;
    }

    if (isTupleAccess) {
      // Tuple field access
      const tupleLayout = this.inferTupleLayout(expr.expr);
      if (!tupleLayout) {
        throw new Error("Cannot determine tuple type for field access");
      }

      if (fieldIndex < 0 || fieldIndex >= tupleLayout.elements.length) {
        throw new Error(`Tuple index out of bounds: ${fieldIndex}`);
      }

      const elemLayout = tupleLayout.elements[fieldIndex];
      builder.i32Load(2, elemLayout.offset);
    } else {
      // Struct field access
      const structLayout = this.inferStructLayout(expr.expr);
      if (!structLayout) {
        throw new Error("Cannot determine struct type for field access");
      }

      const fieldLayout = structLayout.fields.get(expr.field);
      if (!fieldLayout) {
        throw new Error(`Unknown field: ${expr.field}`);
      }

      builder.i32Load(2, fieldLayout.offset);
    }
  }

  private compileArrayExpr(expr: ast.ArrayExpr, needsValue: boolean): void {
    const builder = this.currentFunc!.builder;

    if (expr.elements.length === 0) {
      // Empty array - just a null pointer
      if (needsValue) {
        builder.i32Const(0);
      }
      return;
    }

    // Initialize memory if needed
    this.initMemory();

    // Array layout: [length (i32), element0, element1, ...]
    // All elements assumed to be i32 for now
    const elemSize = 4;
    const headerSize = 4; // length field
    const totalSize = headerSize + expr.elements.length * elemSize;

    // Allocate memory for array (bump allocator)
    builder.globalGet(this.heapPtrGlobal);

    // Save pointer to a local for element writes
    const ptrLocal = this.currentFunc!.paramCount + builder.addLocal(ValType.I32);
    builder.localTee(ptrLocal);

    // Bump heap pointer by array size
    builder.i32Const(totalSize);
    builder.i32Add();
    builder.globalSet(this.heapPtrGlobal);

    // Store length at offset 0
    builder.localGet(ptrLocal);
    builder.i32Const(expr.elements.length);
    builder.i32Store(2, 0);

    // Write each element
    for (let i = 0; i < expr.elements.length; i++) {
      const elem = expr.elements[i];
      const offset = headerSize + i * elemSize;

      // Get base pointer
      builder.localGet(ptrLocal);

      // Compile element value
      this.compileExpr(elem, true);

      // Store at offset
      builder.i32Store(2, offset);
    }

    // Leave pointer on stack if value is needed
    if (needsValue) {
      builder.localGet(ptrLocal);
    }
  }

  private compileIndexExpr(expr: ast.IndexExpr, needsValue: boolean): void {
    const builder = this.currentFunc!.builder;

    // Array layout: [length (i32), element0, element1, ...]
    const headerSize = 4;
    const elemSize = 4;

    // Compile base expression (array pointer)
    this.compileExpr(expr.expr, true);

    // Save array pointer to local
    const arrLocal = this.currentFunc!.paramCount + builder.addLocal(ValType.I32);
    builder.localSet(arrLocal);

    // Compile index expression
    this.compileExpr(expr.index, true);

    // Calculate offset: header_size + index * elem_size
    builder.i32Const(elemSize);
    builder.i32Mul();
    builder.i32Const(headerSize);
    builder.i32Add();

    // Add base address
    builder.localGet(arrLocal);
    builder.i32Add();

    // Load value from calculated address
    if (needsValue) {
      builder.i32Load(2, 0);
    } else {
      builder.drop();
    }
  }

  private compilePathExpr(expr: ast.PathExpr, needsValue: boolean): void {
    const builder = this.currentFunc!.builder;

    // Handle enum variant paths like Status::Ok
    if (expr.segments.length === 2) {
      const [enumName, variantName] = expr.segments;
      const enumLayout = this.enums.get(enumName);

      if (enumLayout) {
        const variant = enumLayout.variants.get(variantName);
        if (!variant) {
          throw new Error(`Unknown enum variant: ${enumName}::${variantName}`);
        }

        if (needsValue) {
          if (enumLayout.isSimple) {
            // Simple enum - just push the tag
            builder.i32Const(variant.tag);
          } else {
            // Enum with data - this is a unit variant in a complex enum
            // Allocate memory and store just the tag for consistency
            this.initMemory();

            // Allocate memory
            builder.globalGet(this.heapPtrGlobal);
            const ptrLocal = this.currentFunc!.paramCount + builder.addLocal(ValType.I32);
            builder.localTee(ptrLocal);

            // Bump heap pointer
            builder.i32Const(enumLayout.size);
            builder.i32Add();
            builder.globalSet(this.heapPtrGlobal);

            // Store tag at offset 0
            builder.localGet(ptrLocal);
            builder.i32Const(variant.tag);
            builder.i32Store(2, 0);

            // Leave pointer on stack
            builder.localGet(ptrLocal);
          }
        }
        return;
      }
    }

    throw new Error(`Unsupported path expression: ${expr.segments.join("::")}`);
  }

  private compileMatchExpr(expr: ast.MatchExpr, needsValue: boolean): void {
    const builder = this.currentFunc!.builder;

    // Compile the expression being matched
    this.compileExpr(expr.expr, true);

    // Store scrutinee in a local so we can use it multiple times
    const scrutineeLocal = this.currentFunc!.paramCount + builder.addLocal(ValType.I32);
    builder.localSet(scrutineeLocal);

    // Determine block type for the match result
    const blockType = needsValue ? ValType.I32 : BLOCK_TYPE_VOID;

    // Generate nested if-else chain for pattern matching
    // Each arm is: if (matches pattern) { body } else { next arm }
    this.compileMatchArms(expr.arms, scrutineeLocal, blockType, needsValue, 0);
  }

  private compileMatchArms(
    arms: ast.MatchArm[],
    scrutineeLocal: number,
    blockType: number,
    needsValue: boolean,
    armIndex: number,
  ): void {
    const builder = this.currentFunc!.builder;

    if (armIndex >= arms.length) {
      // No more arms - unreachable (should be caught by exhaustiveness check)
      builder.unreachable();
      return;
    }

    const arm = arms[armIndex];
    const isLast = armIndex === arms.length - 1;

    // Check if this is a wildcard pattern (matches everything)
    if (arm.pattern.kind === "WildcardPattern") {
      // Just compile the body directly
      this.compileExpr(arm.body, needsValue);
      return;
    }

    // Check if IdentPattern is actually an enum variant (like `None`)
    if (arm.pattern.kind === "IdentPattern") {
      const varName = arm.pattern.name;

      // Check if this identifier is an enum variant
      let enumLayout: EnumLayout | null = null;
      let variant: EnumVariantLayout | null = null;

      for (const [_, layout] of this.enums) {
        const v = layout.variants.get(varName);
        if (v) {
          enumLayout = layout;
          variant = v;
          break;
        }
      }

      if (enumLayout && variant) {
        // This is an enum variant pattern without parentheses (like `None`)
        // Treat it like an EnumPattern
        if (enumLayout.isSimple) {
          builder.localGet(scrutineeLocal);
        } else {
          // Load tag from memory
          builder.localGet(scrutineeLocal);
          builder.i32Load(2, 0);
        }
        builder.i32Const(variant.tag);
        builder.i32Eq();

        builder.if_(blockType);
        this.currentFunc!.blockDepth++;

        this.compileExpr(arm.body, needsValue);

        if (!isLast) {
          builder.else_();
          this.compileMatchArms(arms, scrutineeLocal, blockType, needsValue, armIndex + 1);
        } else if (needsValue) {
          // Last arm needs an else branch for validation if it produces a value
          builder.else_();
          builder.unreachable();
        }

        builder.end();
        this.currentFunc!.blockDepth--;
        return;
      }

      // It's a real binding pattern - always matches and binds the value
      const localIdx = this.currentFunc!.paramCount + builder.addLocal(ValType.I32);
      this.currentFunc!.locals.set(varName, { index: localIdx, type: ValType.I32 });
      builder.localGet(scrutineeLocal);
      builder.localSet(localIdx);

      // Just compile the body directly
      this.compileExpr(arm.body, needsValue);
      return;
    }

    // Check if this is an enum pattern
    if (arm.pattern.kind === "EnumPattern") {
      // Get the enum and variant info
      const variantName = arm.pattern.variant;

      // Try to find which enum this variant belongs to
      let enumLayout: EnumLayout | null = null;
      let variant: EnumVariantLayout | null = null;

      for (const [_, layout] of this.enums) {
        const v = layout.variants.get(variantName);
        if (v) {
          enumLayout = layout;
          variant = v;
          break;
        }
      }

      if (!enumLayout || !variant) {
        throw new Error(`Unknown enum variant in pattern: ${variantName}`);
      }

      // Generate: if (tag == expected) { body } else { next arm }
      // For simple enums, scrutinee IS the tag
      // For enums with data, scrutinee is a pointer and tag is at offset 0
      if (enumLayout!.isSimple) {
        builder.localGet(scrutineeLocal);
      } else {
        // Load tag from memory
        builder.localGet(scrutineeLocal);
        builder.i32Load(2, 0);
      }
      builder.i32Const(variant.tag);
      builder.i32Eq();

      builder.if_(blockType);
      this.currentFunc!.blockDepth++;

      // Bind pattern variables if any
      if (arm.pattern.fields && variant.fields) {
        for (let i = 0; i < arm.pattern.fields.length && i < variant.fields.length; i++) {
          const fieldPattern = arm.pattern.fields[i];
          if (fieldPattern.kind === "IdentPattern") {
            // Extract field from enum data
            const fieldLayout = variant.fields[i];
            const varName = fieldPattern.name;
            const localIdx = this.currentFunc!.paramCount + builder.addLocal(ValType.I32);
            this.currentFunc!.locals.set(varName, { index: localIdx, type: ValType.I32 });

            // Load field from scrutinee pointer
            builder.localGet(scrutineeLocal);
            builder.i32Load(2, fieldLayout.offset);
            builder.localSet(localIdx);
          } else if (fieldPattern.kind === "WildcardPattern") {
            // Ignore this field
          }
        }
      }

      this.compileExpr(arm.body, needsValue);

      if (!isLast) {
        builder.else_();
        this.compileMatchArms(arms, scrutineeLocal, blockType, needsValue, armIndex + 1);
      } else if (needsValue) {
        // Last arm needs an else branch for validation if it produces a value
        builder.else_();
        builder.unreachable();
      }

      builder.end();
      this.currentFunc!.blockDepth--;
      return;
    }

    // Check if this is an integer literal pattern
    if (arm.pattern.kind === "LiteralPattern") {
      const litPattern = arm.pattern as ast.LiteralPattern;

      builder.localGet(scrutineeLocal);

      // Compare with literal value
      if (litPattern.value.kind === "IntLiteral") {
        builder.i32Const(Number(litPattern.value.value));
        builder.i32Eq();
      } else if (litPattern.value.kind === "BoolLiteral") {
        builder.i32Const(litPattern.value.value ? 1 : 0);
        builder.i32Eq();
      } else {
        throw new Error(`Unsupported literal pattern type: ${litPattern.value.kind}`);
      }

      builder.if_(blockType);
      this.currentFunc!.blockDepth++;

      this.compileExpr(arm.body, needsValue);

      if (!isLast) {
        builder.else_();
        this.compileMatchArms(arms, scrutineeLocal, blockType, needsValue, armIndex + 1);
      } else if (needsValue) {
        // Last arm needs an else branch for validation if it produces a value
        builder.else_();
        builder.unreachable();
      }

      builder.end();
      this.currentFunc!.blockDepth--;
      return;
    }

    throw new Error(`Unsupported pattern kind in match: ${arm.pattern.kind}`);
  }

  // Try to infer struct layout from an expression
  private inferStructLayout(expr: ast.Expr): StructLayout | null {
    if (expr.kind === "Identifier") {
      // Look up the variable's struct type
      const local = this.currentFunc!.locals.get(expr.name);
      if (local && local.structName) {
        return this.structs.get(local.structName) ?? null;
      }
    } else if (expr.kind === "StructExpr") {
      return this.structs.get(expr.name) ?? null;
    } else if (expr.kind === "FieldExpr") {
      // Nested field access - need to get the field's type
      // For now, not supported
    }
    return null;
  }

  private inferTupleLayout(expr: ast.Expr): TupleLayout | null {
    if (expr.kind === "Identifier") {
      // Look up the variable's tuple layout
      const local = this.currentFunc!.locals.get(expr.name);
      if (local && local.tupleLayout) {
        return local.tupleLayout;
      }
    } else if (expr.kind === "TupleExpr") {
      // Calculate layout from the tuple expression
      return this.calculateTupleLayout(expr.elements);
    }
    return null;
  }

  // ---------------------------------------------------------------------------
  // STATEMENT COMPILATION
  // ---------------------------------------------------------------------------

  private compileStmt(stmt: ast.Stmt): void {
    switch (stmt.kind) {
      case "LetStmt":
        this.compileLetStmt(stmt);
        break;
      case "ExprStmt":
        // Compile expression, don't keep value on stack
        this.compileExpr(stmt.expr, false);
        break;
      default:
        // Skip other items for now
        break;
    }
  }

  private compileLetStmt(stmt: ast.LetStmt): void {
    const builder = this.currentFunc!.builder;

    // Handle tuple destructuring pattern
    if (stmt.pattern.kind === "TuplePattern") {
      this.compileTupleDestructuring(stmt.pattern, stmt.init, stmt.type);
      return;
    }

    // Get variable name from pattern
    const name = this.getPatternName(stmt.pattern);
    if (!name) {
      throw new Error("Only simple identifier patterns supported in let");
    }

    // Determine type, struct name, and tuple layout
    let valType: ValType;
    let structName: string | undefined;
    let tupleLayout: TupleLayout | undefined;

    if (stmt.type) {
      if (stmt.type.kind === "NamedType" && this.structs.has(stmt.type.name)) {
        // Struct type - stored as i32 pointer
        valType = ValType.I32;
        structName = stmt.type.name;
      } else if (stmt.type.kind === "TupleType" && stmt.type.elements.length > 0) {
        // Tuple type - stored as i32 pointer
        valType = ValType.I32;
        // Calculate tuple layout from type
        tupleLayout = this.calculateTupleLayoutFromType(stmt.type);
      } else {
        valType = this.astTypeToValType(stmt.type);
      }
    } else if (stmt.init && stmt.init.kind === "StructExpr") {
      // Infer struct type from initializer
      valType = ValType.I32;
      structName = stmt.init.name;
    } else if (stmt.init && stmt.init.kind === "TupleExpr" && stmt.init.elements.length > 0) {
      // Infer tuple type from initializer
      valType = ValType.I32;
      tupleLayout = this.calculateTupleLayout(stmt.init.elements);
    } else {
      valType = ValType.I32;
    }

    // Allocate local
    const localIdx = this.currentFunc!.paramCount + builder.addLocal(valType);
    this.currentFunc!.locals.set(name, { index: localIdx, type: valType, structName, tupleLayout });

    // Compile initializer if present
    if (stmt.init) {
      this.compileExpr(stmt.init, true);
      builder.localSet(localIdx);
    }
  }

  private compileTupleDestructuring(
    pattern: ast.TuplePattern,
    init: ast.Expr | undefined,
    typeAnnotation: ast.Type | undefined,
  ): void {
    const builder = this.currentFunc!.builder;

    if (!init) {
      throw new Error("Tuple destructuring requires an initializer");
    }

    // Determine the tuple layout
    let tupleLayout: TupleLayout;
    if (typeAnnotation && typeAnnotation.kind === "TupleType") {
      tupleLayout = this.calculateTupleLayoutFromType(typeAnnotation);
    } else if (init.kind === "TupleExpr") {
      tupleLayout = this.calculateTupleLayout(init.elements);
    } else if (init.kind === "Identifier") {
      // Look up the tuple layout from the variable
      const local = this.currentFunc!.locals.get(init.name);
      if (local && local.tupleLayout) {
        tupleLayout = local.tupleLayout;
      } else {
        throw new Error(`Cannot determine tuple layout for ${init.name}`);
      }
    } else {
      // Try to infer layout from pattern - assume all elements are i32
      // This handles cases like function calls returning tuples
      const elemLayouts: { offset: number; type: ValType }[] = [];
      let offset = 0;
      for (let i = 0; i < pattern.elements.length; i++) {
        elemLayouts.push({ offset, type: ValType.I32 });
        offset += 4;
      }
      tupleLayout = { size: offset || 4, elements: elemLayouts };
    }

    // Compile the initializer (produces a pointer to the tuple)
    this.compileExpr(init, true);
    const ptrLocal = this.currentFunc!.paramCount + builder.addLocal(ValType.I32);
    builder.localSet(ptrLocal);

    // Bind each element in the pattern
    for (let i = 0; i < pattern.elements.length && i < tupleLayout.elements.length; i++) {
      const elemPattern = pattern.elements[i];
      const elemLayout = tupleLayout.elements[i];

      if (elemPattern.kind === "IdentPattern") {
        // Allocate a local for this binding
        const localIdx = this.currentFunc!.paramCount + builder.addLocal(elemLayout.type);
        this.currentFunc!.locals.set(elemPattern.name, { index: localIdx, type: elemLayout.type });

        // Load from tuple memory and store in local
        builder.localGet(ptrLocal);
        builder.i32Load(2, elemLayout.offset);
        builder.localSet(localIdx);
      } else if (elemPattern.kind === "WildcardPattern") {
        // Skip this element
      } else if (elemPattern.kind === "TuplePattern") {
        // TODO: Handle nested tuple patterns
        throw new Error("Nested tuple patterns not yet supported");
      } else {
        throw new Error(`Unsupported pattern in tuple destructuring: ${elemPattern.kind}`);
      }
    }
  }

  private calculateTupleLayoutFromType(type: ast.TupleType): TupleLayout {
    let offset = 0;
    const elemLayouts: { offset: number; type: ValType }[] = [];

    for (const elemType of type.elements) {
      const valType = this.astTypeToValType(elemType);
      const size = this.valTypeSize(valType);
      const alignment = size;

      offset = Math.ceil(offset / alignment) * alignment;
      elemLayouts.push({ offset, type: valType });
      offset += size;
    }

    const totalSize = Math.ceil(offset / 4) * 4;
    return { size: totalSize || 4, elements: elemLayouts };
  }

  // ---------------------------------------------------------------------------
  // TYPE CONVERSION
  // ---------------------------------------------------------------------------

  private astTypeToValType(type: ast.Type): ValType {
    if (type.kind === "NamedType") {
      switch (type.name) {
        case "i32": return ValType.I32;
        case "i64": return ValType.I64;
        case "f32": return ValType.F32;
        case "f64": return ValType.F64;
        case "bool": return ValType.I32;  // bool is i32 in WASM
        default:
          // Check if it's a struct type - structs are pointers (i32)
          if (this.structs.has(type.name)) {
            return ValType.I32;
          }
          // Check if it's an enum type - simple enums are i32 tags
          if (this.enums.has(type.name)) {
            return ValType.I32;
          }
          throw new Error(`Unsupported type: ${type.name}`);
      }
    }

    if (type.kind === "TupleType") {
      if (type.elements.length === 0) {
        // Unit type - no WASM type
        throw new Error("Unit type has no WASM representation");
      }
      // Non-unit tuples are stored in memory, passed as i32 pointers
      return ValType.I32;
    }

    if (type.kind === "ArrayType") {
      // Arrays are stored in memory, passed as i32 pointers
      return ValType.I32;
    }

    throw new Error(`Unsupported type kind: ${type.kind}`);
  }

  // ---------------------------------------------------------------------------
  // HELPERS
  // ---------------------------------------------------------------------------

  private getPatternName(pattern: ast.Pattern): string | null {
    if (pattern.kind === "IdentPattern") {
      return pattern.name;
    }
    return null;
  }
}

// =============================================================================
// CONVENIENCE FUNCTION
// =============================================================================

export function compile(program: ast.Module): Uint8Array {
  const generator = new CodeGenerator();
  return generator.compile(program);
}
