# W65816 LLVM Backend - Known Limitations

This document tracks known limitations and areas for future improvement.

## Phase 1-2: Infrastructure & Pseudo Instructions

### Register Allocation
- **Limited physical registers**: Only A, X, Y available for general use
- **Register spilling**: `storeRegToStackSlot` and `loadRegFromStackSlot` only handle A, X, Y directly. Virtual registers that don't map to these cause failures.
- **Phi nodes with register pressure**: Complex phi nodes requiring spilling will fail (e.g., loops with multiple live values)
- **Indexed stores with argument order conflict**: When `store array[i] = val` has:
  - val in A, idx in X: Works correctly (spills A, computes idx*2, moves to X, reloads val, stores)
  - val in X, idx in A: May fail or generate incorrect code (X gets clobbered by index)
  - Workaround: Order function arguments so value is first (in A) and index is second (in X)

### Pseudo Instruction Expansion
- **ADD16rr/SUB16rr**: Uses stack-relative addressing which adds overhead (push, operate, pull)
- **Stack cleanup**: Some sequences (like ADD A to A) have suboptimal stack handling

## Phase 3: Control Flow

### Condition Code Mapping
- **Signed comparisons (sgt, slt, sge, sle)**: Now working correctly with multi-instruction sequences
  - `SETLT` (N != V): BVS/BMI/BRA/BPL sequence
  - `SETGE` (N == V): BVS/BPL/BRA/BMI sequence
  - `SETGT` (Z==0 && N==V): BEQ then N==V check
  - `SETLE` (Z==1 || N!=V): BEQ then N!=V check
  - Note: Signed SELECT_CC still uses simplified mapping (fallback to BNE)

- **Unsigned comparisons**: Work correctly via carry flag
  - `SETUGE` → BCS (correct)
  - `SETULT` → BCC (correct)

### Comparison Optimization
- **Compare with immediate**: Now uses CMP_imm16 directly for all immediate comparisons including 0. Much more efficient than stack-relative approach.
- **Self-comparison**: Comparing a value with itself generates unnecessary code

### Branch Relaxation
- **8-bit branch limit**: Short branches limited to -128 to +127 bytes
- **Fallback to JMP**: Works but JMP is 3 bytes vs 2 for short branch

## General Architecture Limitations

### No Hardware Multiply/Divide
- MUL, DIV, REM all expand to library calls (not yet implemented)
- Shift by constant amount: Works! Expands to multiple ASL/LSR/ROR instructions
- Shift by variable amount: Works! Uses loop with DEX/BNE

### Shift/Rotate Instructions
**Accumulator mode (working):**
- `ASL A`, `LSR A`, `ROL A`, `ROR A` - shift/rotate accumulator

**Memory modes (defined, no selection patterns):**
- Direct page: `ASL $dp`, `LSR $dp`, `ROL $dp`, `ROR $dp`
- DP indexed X: `ASL $dp,x`, `LSR $dp,x`, `ROL $dp,x`, `ROR $dp,x`
- Absolute: `ASL $addr`, `LSR $addr`, `ROL $addr`, `ROR $addr`
- Absolute indexed X: `ASL $addr,x`, `LSR $addr,x`, `ROL $addr,x`, `ROR $addr,x`

Note: Memory shift/rotate instructions are defined but require intrinsics or
manual assembly. The compiler uses load-shift-store sequences.

**Shift type legalization (FIXED):**
Shift amounts now use i16 consistently. `getScalarShiftAmountTy()` returns MVT::i16
(matching the fact that i8 is not a legal type), and all shift patterns use i16.

### BIT Instruction
**All modes defined:**
- Immediate: `BIT #$const` - tests bits, only sets Z flag
- Absolute: `BIT $addr` - sets Z from A AND mem, N/V from memory bits
- Direct page: `BIT $dp`
- Absolute indexed X: `BIT $addr,x`
- DP indexed X: `BIT $dp,x`

Note: BIT is a specialized instruction that tests bits without modifying the
accumulator. It's typically used before conditional branches.

### INC/DEC Instructions
**Accumulator mode (working):**
- `INC A`, `DEC A` - increment/decrement accumulator

**Memory modes (defined, no selection patterns):**
- Absolute: `INC $addr`, `DEC $addr`
- Direct page: `INC $dp`, `DEC $dp`
- Absolute indexed X: `INC $addr,x`, `DEC $addr,x`
- DP indexed X: `INC $dp,x`, `DEC $dp,x`

Note: Memory increment/decrement instructions are defined but require intrinsics
or manual assembly. The compiler uses load-add-store sequences.

### STZ - Store Zero to Memory
**All modes defined:**
- Absolute: `STZ $addr`
- Direct page: `STZ $dp`
- Absolute indexed X: `STZ $addr,x`
- DP indexed X: `STZ $dp,x`

STZ stores zero to memory without needing to load a register first. Useful for
clearing memory locations efficiently.

### 8/16-bit Mode Switching
- Currently assumes 16-bit mode throughout
- No support for dynamic M/X flag changes
- 8-bit operations use sub-registers but mode switching not implemented

### Addressing Modes
**Working:**
- Absolute addressing: `lda addr`, `sta addr`
- Constant offset: `lda addr+6` (address folding)
- Stack-relative: `lda 1,s`, `sta 1,s`
- Indexed absolute X: `lda addr,x`, `sta addr,x` (for variable array access)
- Indexed absolute Y: `lda addr,y`, `sta addr,y` (selected when index is in Y)
- Direct page addressing: `lda dp`, `sta dp` (for globals in `.zeropage` section)
- Indexed direct page X: `lda dp,x`, `sta dp,x` (for zero page arrays with X index)

**Not Yet Implemented:**
- Indirect addressing ((dp) / (dp,X) / (dp),Y)
- Long addressing (24-bit, cross-bank)
- Stack-relative indirect ((offset,S),Y)

**Note:** W65816 has no `lda dp,y` or `sta dp,y` instructions - only X-indexed direct page exists.
For Y-indexed access to zero page arrays, absolute addressing is used as fallback.

### Calling Convention
- First 3 arguments: passed in A, X, Y registers
- Additional arguments: passed on the stack (implemented!)
- No vararg support
- 32-bit return values (A:X pair) not tested

**Note:** Stack argument offsets may need fine-tuning. The current implementation uses
reserved call frame (space pre-allocated in prologue) for outgoing arguments.

## Phase 5: Frame Lowering Status

### Working
- Function prologues (stack allocation via PHA or TSX/TXS arithmetic)
- Function epilogues (stack deallocation)
- Register spilling via stack-relative addressing (LDA_sr/STA_sr)
- eliminateFrameIndex for spill slots

### Fixed
- ~~User allocas~~ - Now working! Custom DAG selection detects frame index loads/stores and emits LDA_sr/STA_sr
  - `store i16 %x, ptr %local` correctly generates `sta 1,s` (stack-relative)

### Code Generated for Spills
```asm
; Spill to stack slot 3:
sta 3,s                             ; 2-byte Folded Spill

; Reload from stack slot 3:
lda 3,s                             ; 2-byte Folded Reload
```

### Prologue/Epilogue Examples
```asm
; Small stack (<=8 bytes): use PHA/PLA
pha
pha           ; allocate 4 bytes
...
pla
pla           ; deallocate 4 bytes

; Large stack: use TSX/arithmetic/TXS
; (preserves return value in A)
```

## Priority Fixes

### High Priority
1. ~~Register spilling for virtual registers~~ (DONE - works for physical regs)
2. ~~Proper signed comparison sequences~~ (DONE - multi-instruction N!=V/N==V check for branches)
3. ~~Stack frame management (prologue/epilogue)~~ (DONE)
4. ~~User alloca stack-relative addressing~~ (DONE)

### Medium Priority
1. ~~Direct page addressing mode~~ (DONE - for globals with `.zeropage` section)
2. ~~Indexed addressing for arrays~~ (DONE)
3. ~~Compare-with-immediate optimization~~ (DONE - uses CMP_imm16 directly)
4. ~~Register-to-register AND/OR/XOR~~ (DONE - uses stack-relative addressing)
5. ~~Shift by variable amount~~ (DONE - uses loop with DEX/BNE)

### Low Priority
1. 8-bit mode support
2. Long (24-bit) addressing
3. Interrupt handling

## Test Cases That Fail

```llvm
; Fails: register spilling needed
define i16 @count_down(i16 %n) {
entry:
  br label %loop
loop:
  %val = phi i16 [ %n, %entry ], [ %dec, %loop ]
  %dec = add i16 %val, -1
  %done = icmp eq i16 %dec, 0
  br i1 %done, label %exit, label %loop
exit:
  ret i16 %val
}
```

## Completed Phases

- [x] Phase 1: Infrastructure Foundation
- [x] Phase 2: Basic Instruction Selection (arithmetic, logical)
- [x] Phase 3: Control Flow (branches, calls, returns)
- [x] Phase 4: Memory Addressing Modes (indexed array access works!)
- [x] Phase 5: Frame Lowering & ABI (complete - spills and user allocas work)
- [x] Phase 6: MC Layer & Code Emission (ELF object file generation works!)

## Phase 4: Memory Addressing Modes Status

### Working
- Absolute addressing: `lda global_array`, `sta global_array`
- Constant offset addressing: `lda global_array+6` (address folding)
- Stack-relative addressing: `lda 1,s`, `sta 3,s`
- Indexed addressing: `lda addr,x`, `sta addr,x` (variable index array access)

### Code Generated for Variable Index Array Access
```asm
; array_load(i16 %idx):
asl a               ; idx * 2
tax                 ; put index in X
lda global_array,x  ; indexed load
rts

; array_store(i16 %idx, i16 %val):
pha                 ; allocate stack space
sta 1,s             ; spill value
asl a               ; idx * 2
tax                 ; put index in X
lda 1,s             ; reload value
sta global_array,x  ; indexed store
pla                 ; restore stack
rts
```

### Implemented but Untested Patterns
- Indirect: `lda (dp)`, `sta (dp),y` (instructions exist, no patterns yet)

### Code Generated for Direct Page Access
```asm
; For globals with section ".zeropage":
@zp_var = global i16 42, section ".zeropage"

; read_zp() generates:
lda zp_var               ; 2-byte instruction (opcode 0xA5)
rts

; write_zp(val) generates:
sta zp_var               ; 2-byte instruction (opcode 0x85)
rts

; Compare with normal globals using 3-byte absolute addressing:
lda normal_var           ; 3-byte instruction (opcode 0xAD)
```
