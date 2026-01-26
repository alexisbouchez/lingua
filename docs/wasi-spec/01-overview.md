# WebAssembly System Interface (WASI) Overview

## About WASI

WASI is a collection of APIs undergoing standardization by the WASI Subgroup within the WebAssembly Community Group. The initiative draws primarily from POSIX and CloudABI design principles.

## Key Versions

### Preview 1 (Legacy)
- Original iteration
- Used the witx Interface Definition Language
- Achieved widespread adoption
- Single monolithic API surface

### Preview 2 (Current/Stable)
- Now stable and recommended
- Uses the WIT (WebAssembly Interface Types) IDL
- Modular API collection
- Enhanced type system
- Better language support
- Virtualizability support

## WASI Preview 2 Packages

### Core I/O (`wasi:io`)
- `poll` - Async I/O polling
- `streams` - Input/output streams
- `error` - Error handling

### Clocks (`wasi:clocks`)
- `wall-clock` - Real-world time
- `monotonic-clock` - Elapsed time measurement

### Random (`wasi:random`)
- `random` - Cryptographically secure random bytes
- `insecure` - Fast pseudo-random (non-crypto)
- `insecure-seed` - Deterministic seeding

### Filesystem (`wasi:filesystem`)
- `types` - Filesystem types and errors
- `preopens` - Pre-opened directories

### Sockets (`wasi:sockets`)
- `tcp` - TCP networking
- `udp` - UDP networking
- `ip-name-lookup` - DNS resolution
- `network` - Network configuration

### CLI (`wasi:cli`)
- `command` - Command-line application world
- `environment` - Environment variables
- `stdin`, `stdout`, `stderr` - Standard I/O
- `terminal-*` - Terminal handling

### HTTP (`wasi:http`)
- `types` - HTTP types
- `incoming-handler` - Server-side handling
- `outgoing-handler` - Client-side requests

## Repository Structure

Individual API development occurs in separate repositories:
- `WebAssembly/wasi-io`
- `WebAssembly/wasi-clocks`
- `WebAssembly/wasi-random`
- `WebAssembly/wasi-filesystem`
- `WebAssembly/wasi-sockets`
- `WebAssembly/wasi-cli`
- `WebAssembly/wasi-http`

The main `WebAssembly/WASI` repository serves as:
- Hub for general discussion
- Process documentation
- High-level strategic objectives

## Design Principles

1. **Capability-based security**: Resources accessed via handles, not ambient authority
2. **Portability**: Works across Unix-family and Windows platforms
3. **Modularity**: Import only needed interfaces
4. **Virtualizability**: Implementations can be composed and virtualized
5. **Language neutrality**: Works with any language targeting WebAssembly

## Resources

- Main Repository: https://github.com/WebAssembly/WASI
- Proposals: https://github.com/WebAssembly/WASI/blob/main/Proposals.md
- Contributing: https://github.com/WebAssembly/WASI/blob/main/Contributing.md
