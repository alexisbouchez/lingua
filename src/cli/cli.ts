/**
 * Lingua CLI
 *
 * Command-line interface for the Lingua compiler.
 */

import { Lexer } from "../lexer/lexer.ts";
import { Parser } from "../parser/parser.ts";
import { typeCheck } from "../types/checker.ts";
import { compile } from "../codegen/codegen.ts";

// =============================================================================
// CLI COMMANDS
// =============================================================================

async function build(inputPath: string, outputPath?: string): Promise<boolean> {
  // Read source file
  let source: string;
  try {
    source = await Deno.readTextFile(inputPath);
  } catch (e) {
    console.error(`Error reading file: ${inputPath}`);
    console.error(e.message);
    return false;
  }

  // Lexing
  const lexer = new Lexer(source);
  const { tokens, errors: lexErrors } = lexer.tokenize();

  if (lexErrors.length > 0) {
    console.error("Lexer errors:");
    for (const err of lexErrors) {
      console.error(`  ${err.message} at line ${err.span.start.line}:${err.span.start.column}`);
    }
    return false;
  }

  // Parsing
  const parser = new Parser(tokens);
  const { module, errors: parseErrors } = parser.parse();

  if (parseErrors.length > 0) {
    console.error("Parse errors:");
    for (const err of parseErrors) {
      console.error(`  ${err.message} at line ${err.span.start.line}:${err.span.start.column}`);
    }
    return false;
  }

  // Type checking
  const { errors: typeErrors } = typeCheck(module);

  if (typeErrors.length > 0) {
    console.error("Type errors:");
    for (const err of typeErrors) {
      console.error(`  ${err.message} at line ${err.span.start.line}:${err.span.start.column}`);
    }
    return false;
  }

  // Code generation
  let wasm: Uint8Array;
  try {
    wasm = compile(module);
  } catch (e) {
    console.error("Code generation error:");
    console.error(`  ${e.message}`);
    return false;
  }

  // Write output
  const outPath = outputPath ?? inputPath.replace(/\.lingua$/, ".wasm");
  try {
    await Deno.writeFile(outPath, wasm);
    console.log(`Compiled ${inputPath} -> ${outPath} (${wasm.length} bytes)`);
  } catch (e) {
    console.error(`Error writing file: ${outPath}`);
    console.error(e.message);
    return false;
  }

  return true;
}

async function run(inputPath: string): Promise<boolean> {
  // First build
  const wasmPath = inputPath.replace(/\.lingua$/, ".wasm");
  const success = await build(inputPath, wasmPath);
  if (!success) {
    return false;
  }

  // Run with wasmtime
  console.log(`Running ${wasmPath}...`);
  const cmd = new Deno.Command("wasmtime", {
    args: [wasmPath],
    stdout: "inherit",
    stderr: "inherit",
  });

  try {
    const result = await cmd.output();
    return result.success;
  } catch (e) {
    console.error("Error running wasmtime:");
    console.error(`  ${e.message}`);
    console.error("Make sure wasmtime is installed: https://wasmtime.dev/");
    return false;
  }
}

async function check(inputPath: string): Promise<boolean> {
  // Read source file
  let source: string;
  try {
    source = await Deno.readTextFile(inputPath);
  } catch (e) {
    console.error(`Error reading file: ${inputPath}`);
    console.error(e.message);
    return false;
  }

  // Lexing
  const lexer = new Lexer(source);
  const { tokens, errors: lexErrors } = lexer.tokenize();

  if (lexErrors.length > 0) {
    console.error("Lexer errors:");
    for (const err of lexErrors) {
      console.error(`  ${err.message} at line ${err.span.start.line}:${err.span.start.column}`);
    }
    return false;
  }

  // Parsing
  const parser = new Parser(tokens);
  const { module, errors: parseErrors } = parser.parse();

  if (parseErrors.length > 0) {
    console.error("Parse errors:");
    for (const err of parseErrors) {
      console.error(`  ${err.message} at line ${err.span.start.line}:${err.span.start.column}`);
    }
    return false;
  }

  // Type checking
  const { errors: typeErrors } = typeCheck(module);

  if (typeErrors.length > 0) {
    console.error("Type errors:");
    for (const err of typeErrors) {
      console.error(`  ${err.message} at line ${err.span.start.line}:${err.span.start.column}`);
    }
    return false;
  }

  console.log(`${inputPath}: OK`);
  return true;
}

// =============================================================================
// MAIN
// =============================================================================

function printUsage(): void {
  console.log(`
Lingua Compiler

Usage:
  lingua build <file.lingua> [output.wasm]   Compile to WASM
  lingua run <file.lingua>                   Compile and run with wasmtime
  lingua check <file.lingua>                 Type-check without compiling
  lingua help                                Show this help

Examples:
  lingua build hello.lingua                  # Creates hello.wasm
  lingua build hello.lingua out.wasm         # Creates out.wasm
  lingua run hello.lingua                    # Compile and run
  lingua check hello.lingua                  # Check for errors
`);
}

async function main(): Promise<void> {
  const args = Deno.args;

  if (args.length === 0) {
    printUsage();
    Deno.exit(1);
  }

  const command = args[0];

  switch (command) {
    case "build": {
      if (args.length < 2) {
        console.error("Error: Missing input file");
        printUsage();
        Deno.exit(1);
      }
      const success = await build(args[1], args[2]);
      Deno.exit(success ? 0 : 1);
      break;
    }

    case "run": {
      if (args.length < 2) {
        console.error("Error: Missing input file");
        printUsage();
        Deno.exit(1);
      }
      const success = await run(args[1]);
      Deno.exit(success ? 0 : 1);
      break;
    }

    case "check": {
      if (args.length < 2) {
        console.error("Error: Missing input file");
        printUsage();
        Deno.exit(1);
      }
      const success = await check(args[1]);
      Deno.exit(success ? 0 : 1);
      break;
    }

    case "help":
    case "--help":
    case "-h":
      printUsage();
      break;

    default:
      console.error(`Unknown command: ${command}`);
      printUsage();
      Deno.exit(1);
  }
}

main();
