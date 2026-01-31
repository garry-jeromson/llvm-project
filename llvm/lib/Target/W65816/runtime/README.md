# W65816 Runtime Library

This directory contains runtime support functions required by the W65816 LLVM backend.

## Functions Provided

### Arithmetic

| Function | Operation | Description |
|----------|-----------|-------------|
| `__mulhi3` | `A * X` | 16-bit unsigned multiplication |
| `__divhi3` | `A / X` | 16-bit signed division |
| `__udivhi3` | `A / X` | 16-bit unsigned division |
| `__modhi3` | `A % X` | 16-bit signed remainder |
| `__umodhi3` | `A % X` | 16-bit unsigned remainder |

### Memory Operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `memcpy` | `memcpy(dest, src, n)` | Copy n bytes from src to dest |
| `memset` | `memset(dest, c, n)` | Fill n bytes at dest with value c |
| `memmove` | `memmove(dest, src, n)` | Copy n bytes, handles overlap |

## Calling Convention

### Arithmetic Functions
- **First argument**: A register (16-bit)
- **Second argument**: X register (16-bit)
- **Return value**: A register (16-bit)

### Memory Functions
- **dest**: A register (16-bit pointer)
- **src/value**: X register (16-bit pointer or byte value)
- **count**: Y register (16-bit byte count)
- **Return value**: A register (original dest pointer)

Functions may clobber A, X, Y, and processor flags. The direct page (D) and data bank (DBR) registers are preserved.

## Building

### With ca65 (cc65 toolchain)

```bash
ca65 --cpu 65816 -o w65816_runtime.o w65816_runtime.s
```

### With WLA-DX

The source uses ca65 syntax. For WLA-DX, you'll need to adapt the directives:
- `.proc` → section labels
- `.endproc` → (remove)
- `.export` → `.export` or `.global`
- `.bss` / `.code` → `.ramsection` / `.section`

### With other assemblers

The algorithms are standard and can be ported to any 65816 assembler. Key points:
- Assumes 16-bit accumulator and index registers (native mode)
- Uses 6 bytes of temporary storage (can be in direct page for better performance)

## Linking

Link the runtime object file with your compiled code:

```bash
# Compile your program
llc -march=w65816 -filetype=obj program.ll -o program.o

# Link with runtime (using your linker of choice)
ld65 -o program.bin program.o w65816_runtime.o -C your_linker_config.cfg
```

## Performance Notes

- **Multiplication**: O(16) iterations, shift-and-add algorithm
- **Division/Remainder**: O(16) iterations, binary long division

For performance-critical code, consider:
1. Using bit shifts for powers of 2 (`x * 4` → `x << 2`)
2. Lookup tables for common multiplications
3. SNES hardware multiply registers ($4202-$4217) for 8x8 or 16x8 multiply

## SNES Hardware Multiply/Divide

The SNES has hardware multiply/divide registers that can be faster for specific cases:

```asm
; 8x8 unsigned multiply (result ready after 8 cycles)
sta $4202       ; First factor (8-bit)
stx $4203       ; Second factor (8-bit) - starts multiplication
nop             ; Wait for result
nop
lda $4216       ; Read 16-bit result

; 16/8 unsigned divide (result ready after 16 cycles)
sta $4204       ; Dividend low
stx $4205       ; Dividend high
sty $4206       ; Divisor (8-bit) - starts division
; ... wait 16 cycles ...
lda $4214       ; Quotient
ldx $4216       ; Remainder
```

These are NOT used by the runtime library because:
1. They require specific timing/waiting
2. 8-bit divisor limitation for divide
3. The runtime needs to work on all W65816 systems, not just SNES

## Memory Requirements

- **Code**: ~400 bytes
- **Data**: 6 bytes of temporary storage (in `.bss` section)
- **Zero page**: 6 bytes for pointer operations (required by memcpy/memset/memmove)

## Testing the Runtime Library

A self-contained test suite is provided that can run in any 65816 emulator.

### Building the Tests

```bash
# Assemble the test and runtime
ca65 --cpu 65816 -o test_runtime.o test_runtime.s
ca65 --cpu 65816 -o w65816_runtime.o w65816_runtime.s

# Link into a binary (loads at $8000)
ld65 -o test_runtime.bin test_runtime.o w65816_runtime.o -C test_runtime.cfg
```

### Running the Tests

Load `test_runtime.bin` at address $8000 in any 65816 emulator and execute from $8000.

The test runs 49 test cases and stores results in zero page:

| Address | Contents |
|---------|----------|
| $0000 | Total tests run |
| $0002 | Tests passed |
| $0004 | Tests failed |
| $0006 | Status: `0x600D` = all pass, `0xFA11` = fail |

### Emulator Options

The test binary is compatible with:

- **bsnes/higan**: Load as raw binary, set PC to $8000
- **Mesen-S**: Use "Load ROM" or debug features
- **no$snes**: Load and run from $8000
- **65816 CPU simulators**: Any simulator supporting native mode

### Expected Output

After execution completes (enters infinite loop):
- `$0000` = 49 (0x0031) - total tests
- `$0002` = 49 (0x0031) - passed
- `$0004` = 0 - failed
- `$0006` = 0x600D - all tests passed

### Test Coverage

| Function | Tests | Cases |
|----------|-------|-------|
| `__mulhi3` | 8 | 0×0, 1×1, 5×7, commutative, 100×100, overflow, 255×255, large |
| `__udivhi3` | 8 | 0/1, 10/2, truncation, max values, large numbers |
| `__divhi3` | 8 | positive, negative dividend, negative divisor, both negative, truncation toward zero |
| `__umodhi3` | 8 | 0%1, basic, exact division, various sizes |
| `__modhi3` | 8 | positive, sign-of-dividend cases, edge cases |
| `memcpy` | 3 | 0 bytes, 4 bytes, 1 byte |
| `memset` | 3 | 0 bytes, 4 bytes fill, zero fill |
| `memmove` | 3 | non-overlapping, overlapping (backward), edge case |
