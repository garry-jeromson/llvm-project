# W65816 Backend

This backend provides code generation for the Western Design Center W65816
16-bit microprocessor, famously used in the Super Nintendo Entertainment
System (SNES) and Apple IIGS.

## Status

The backend is functional for C compilation targeting SNES/homebrew development.
All 94 W65816 instructions are implemented, with full support for the 24
addressing modes.

## Key Features

- Native 16-bit code generation (native mode, not emulation mode)
- Full Clang integration: `clang -target w65816-unknown-none`
- Calling convention: first 3 args in A, X, Y; rest on stack
- Stack-relative and direct page addressing
- Interrupt handler support (`__attribute__((interrupt))`)
- Far function calls (`__attribute__((w65816_farfunc))`)
- Direct page frame allocation for faster local access

## Known Limitations

- **32/64-bit integers**: Not supported in function signatures (clear error)
- **-O0**: Explicitly blocked (3-register architecture requires optimization)
- **Mutual recursion**: Not detected; may produce incorrect results
- **Register pressure**: Complex expressions may fail to allocate

## Documentation

See `llvm/docs/W65816Usage.rst` for comprehensive user documentation including
calling conventions, inline assembly, and addressing modes.

## Testing

```bash
# Run backend tests
llvm-lit llvm/test/CodeGen/W65816/

# Compile example
clang -target w65816-unknown-none -O2 -S test.c -o test.s
```
