LINGUA - WASM/WASI SPECIFICATION REFERENCE
===========================================

WEBASSEMBLY CORE SPECIFICATIONS
===============================

WebAssembly 3.0 (Current - Released 2025-09-17)
-----------------------------------------------
Core Specification (Multi-page): https://webassembly.github.io/spec/core/
Core Specification (PDF):        https://webassembly.github.io/spec/core/_download/WebAssembly.pdf
W3C Single-page Version:         https://www.w3.org/TR/wasm-core-2/

Key sections:
- Introduction:    https://webassembly.github.io/spec/core/intro/index.html
- Structure:       https://webassembly.github.io/spec/core/syntax/index.html
- Validation:      https://webassembly.github.io/spec/core/valid/index.html
- Execution:       https://webassembly.github.io/spec/core/exec/index.html
- Binary Format:   https://webassembly.github.io/spec/core/binary/index.html
- Text Format:     https://webassembly.github.io/spec/core/text/index.html
- Appendix:        https://webassembly.github.io/spec/core/appendix/index.html

WASM 3.0 New Features:
- 64-bit address space (i64 memories/tables)
- Garbage collection (struct/array types)
- Deterministic execution
- Custom annotation syntax

JavaScript Embedding API
------------------------
W3C Specification: https://www.w3.org/TR/wasm-js-api-2/
GitHub:            https://webassembly.github.io/spec/js-api/

Web Embedding API
-----------------
W3C Specification: https://www.w3.org/TR/wasm-web-api-2/
GitHub:            https://webassembly.github.io/spec/web-api/


WASI SPECIFICATIONS
===================

WASI 0.2 (Current Stable)
-------------------------
Main Documentation:  https://wasi.dev/
Interfaces Overview: https://wasi.dev/interfaces
GitHub Repository:   https://github.com/WebAssembly/WASI
Preview2 Docs:       https://github.com/WebAssembly/WASI/blob/main/docs/Preview2.md

WASI 0.2 Interfaces:
- Clocks:     https://github.com/WebAssembly/wasi-clocks
- Random:     https://github.com/WebAssembly/wasi-random
- Filesystem: https://github.com/WebAssembly/wasi-filesystem
- Sockets:    https://github.com/WebAssembly/wasi-sockets
- CLI:        https://github.com/WebAssembly/wasi-cli
- HTTP:       https://github.com/WebAssembly/wasi-http
- I/O:        https://github.com/WebAssembly/wasi-io

WASI 0.3 (Draft - Upcoming)
---------------------------
Clocks:     https://github.com/WebAssembly/WASI/tree/main/proposals/clocks/wit-0.3.0-draft
Random:     https://github.com/WebAssembly/WASI/tree/main/proposals/random/wit-0.3.0-draft
Filesystem: https://github.com/WebAssembly/WASI/tree/main/proposals/filesystem/wit-0.3.0-draft
Sockets:    https://github.com/WebAssembly/WASI/tree/main/proposals/sockets/wit-0.3.0-draft
CLI:        https://github.com/WebAssembly/WASI/tree/main/proposals/cli/wit-0.3.0-draft
HTTP:       https://github.com/WebAssembly/WASI/tree/main/proposals/http/wit-0.3.0-draft

WASI 0.1 (Legacy/Preview1)
--------------------------
Branch:     https://github.com/WebAssembly/WASI/tree/wasi-0.1
Uses WITX IDL format (deprecated, use WIT for new projects)

All WASI Proposals
------------------
Proposals List: https://github.com/WebAssembly/WASI/blob/main/docs/Proposals.md

Phase 2 Proposals:
- Machine Learning (wasi-nn): https://github.com/WebAssembly/wasi-nn
- Clocks Timezone
- GFX

Phase 1 Proposals:
- Blob Store, Crypto, Digital I/O, Distributed Lock Service, I2C,
  Key-value Store, Logging, Messaging, Observe, Parallel, Pattern Match,
  Runtime Config, SPI, SQL, SQL Embed, Threads, URL, USB, TLS


WEBASSEMBLY COMPONENT MODEL
===========================

Component Model Documentation: https://component-model.bytecodealliance.org/
Component Model Spec:          https://github.com/WebAssembly/component-model
WIT Format Docs:               https://github.com/WebAssembly/WASI/blob/main/docs/WitInWasi.md
Proposal Template:             https://github.com/WebAssembly/wasi-proposal-template

Component Model Topics:
- Why Components:      https://component-model.bytecodealliance.org/design/why-component-model.html
- Core Concepts:       https://component-model.bytecodealliance.org/design/components.html
- WIT Specification:   https://component-model.bytecodealliance.org/design/wit.html
- Building in Go:      https://component-model.bytecodealliance.org/language-support/go.html


GITHUB REPOSITORIES
===================

Core Specifications:
- WebAssembly Spec:    https://github.com/WebAssembly/spec
- WASI:                https://github.com/WebAssembly/WASI
- Component Model:     https://github.com/WebAssembly/component-model

Tools & Runtimes (Go-compatible):
- wazero (pure Go):    https://github.com/tetratelabs/wazero (https://wazero.io/)
- Wasmtime:            https://github.com/bytecodealliance/wasmtime (https://wasmtime.dev/)
- WasmEdge:            https://github.com/WasmEdge/WasmEdge (https://wasmedge.org/)
- Wasmer:              https://github.com/wasmerio/wasmer (https://wasmer.io/)
- WAMR:                https://github.com/bytecodealliance/wasm-micro-runtime


DEVELOPER RESOURCES
===================

MDN WebAssembly Docs:  https://developer.mozilla.org/en-US/docs/WebAssembly
WebAssembly.org:       https://webassembly.org/
WebAssembly Specs:     https://webassembly.org/specs/

Community:
- WASI Meetings:       https://github.com/WebAssembly/meetings/tree/main/wasi
- Bytecode Alliance:   https://bytecodealliance.org/
- Zulip Chat:          https://bytecodealliance.zulipchat.com/


GO-SPECIFIC RESOURCES
=====================

wazero Documentation:  https://wazero.io/docs/
wazero Examples:       https://github.com/tetratelabs/wazero/tree/main/examples
Go WASM/WASI Guide:    https://component-model.bytecodealliance.org/language-support/go.html
