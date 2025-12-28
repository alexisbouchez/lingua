#!/bin/bash
set -e

# Test Lingua compiler with Wasmtime runtime
echo "Testing Lingua with Wasmtime..."
echo

# Build the compiler
echo "Building lingua compiler..."
go build -o lingua ./cmd/lingua
echo

# Add wasmtime to PATH
export PATH="$HOME/.wasmtime/bin:$PATH"

# Check wasmtime is available
if ! command -v wasmtime &> /dev/null; then
    echo "Error: wasmtime not found in PATH"
    exit 1
fi

echo "Wasmtime version:"
wasmtime --version
echo

# Test 1: Hello World
echo "Test 1: Hello World"
./lingua examples/hello.lingua /tmp/hello.wasm
output=$(wasmtime /tmp/hello.wasm 2>&1 | head -1)
if [ "$output" == "Hello, World!" ]; then
    echo "✓ Hello World test passed"
else
    echo "✗ Hello World test failed: expected 'Hello, World!', got '$output'"
    exit 1
fi
echo

# Test 2: FizzBuzz (just check it runs without error)
echo "Test 2: FizzBuzz"
./lingua examples/fizzbuzz.lingua /tmp/fizzbuzz.wasm
output=$(wasmtime /tmp/fizzbuzz.wasm 2>&1 | grep -c "FizzBuzz" || true)
if [ "$output" -gt 0 ]; then
    echo "✓ FizzBuzz test passed"
else
    echo "✗ FizzBuzz test failed"
    exit 1
fi
echo

# Test 3: Factorial
echo "Test 3: Factorial"
./lingua examples/factorial.lingua /tmp/factorial.wasm
output=$(wasmtime /tmp/factorial.wasm 2>&1 | head -1)
expected="1"  # factorial(1) = 1
if [ "$output" == "$expected" ]; then
    echo "✓ Factorial test passed"
else
    echo "✗ Factorial test failed: expected '$expected', got '$output'"
    exit 1
fi
echo

# Test 4: Countdown
echo "Test 4: Countdown"
./lingua examples/countdown.lingua /tmp/countdown.wasm
output=$(wasmtime /tmp/countdown.wasm 2>&1 | head -1)
if [ "$output" == "10" ]; then
    echo "✓ Countdown test passed"
else
    echo "✗ Countdown test failed: expected '10', got '$output'"
    exit 1
fi
echo

# Cleanup
rm -f /tmp/hello.wasm /tmp/fizzbuzz.wasm /tmp/factorial.wasm /tmp/countdown.wasm lingua

echo "All Wasmtime tests passed! ✓"
