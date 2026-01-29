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
- **ADD16ri/SUB16ri**: Immediate variants work correctly - `CLC; ADC #imm` / `SEC; SBC #imm`
- **Stack cleanup**: Some sequences (like ADD A to A) have suboptimal stack handling

## Phase 3: Control Flow

### Condition Code Mapping
- **Signed comparisons (sgt, slt, sge, sle)**: Now working correctly with multi-instruction sequences
  - `SETLT` (N != V): BVS/BMI/BRA/BPL sequence
  - `SETGE` (N == V): BVS/BPL/BRA/BMI sequence
  - `SETGT` (Z==0 && N==V): BEQ then N==V check
  - `SETLE` (Z==1 || N!=V): BEQ then N!=V check
  - Signed SELECT_CC also works via Select16_SLT/SGE/SGT/SLE pseudos

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
- MUL, DIV, REM expand to library calls (implemented!)
  - MUL i16 → `__mulhi3`
  - SDIV i16 → `__divhi3`
  - UDIV i16 → `__udivhi3`
  - SREM i16 → `__modhi3`
  - UREM i16 → `__umodhi3`
- A runtime library is provided in `lib/Target/W65816/runtime/w65816_runtime.s`
  - Assemble with ca65: `ca65 --cpu 65816 -o w65816_runtime.o w65816_runtime.s`
  - Link the object file with your compiled code
  - See `runtime/README.md` for details
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
**All modes defined with selection patterns:**
- Absolute: `STZ $addr` - **has selection pattern** for `store i16 0, ptr @global`
- Direct page: `STZ $dp` - **has selection pattern** for `.zeropage` globals
- Absolute indexed X: `STZ $addr,x` - **has selection pattern** for indexed zero stores
- DP indexed X: `STZ $dp,x` - **has selection pattern** (uses absolute variant)

STZ stores zero to memory without needing to load a register first. Useful for
clearing memory locations efficiently.

### XBA - Exchange B and A (Byte Swap)
**Selection pattern implemented:**
- `XBA` instruction maps to `llvm.bswap.i16` intrinsic
- Swaps high and low bytes of 16-bit accumulator in single instruction
- Example: `call i16 @llvm.bswap.i16(i16 %val)` → `xba`

### Block Move Instructions (MVN/MVP)
**Both instructions defined:**
- `MVN srcbank,destbank` - Block Move Negative (incrementing addresses)
- `MVP srcbank,destbank` - Block Move Positive (decrementing addresses)

Usage: Load X with source address, Y with dest address, C (accumulator) with length-1.
MVN copies from low to high addresses, MVP copies from high to low.
Use MVN when dest < source, MVP when dest > source (for overlapping blocks).

### JMP Indirect Modes
**All indirect jump modes defined:**
- `JMP ($addr)` - Jump Absolute Indirect (through 16-bit pointer)
- `JMP ($addr,X)` - Jump Absolute Indexed Indirect (jump tables)
- `JMP [$addr]` - Jump Absolute Indirect Long (through 24-bit pointer)

### XCE - Exchange Carry and Emulation
The XCE instruction (0xFB) exchanges carry and emulation bits.
- To enter native mode: `CLC; XCE`
- To enter emulation mode: `SEC; XCE`

### 65816-Specific Instructions (All Defined)
**Inter-register transfers:**
- `TXY` - Transfer X to Y
- `TYX` - Transfer Y to X

**16-bit register transfers:**
- `TCD` - Transfer Accumulator to Direct Page register
- `TDC` - Transfer Direct Page register to Accumulator
- `TCS` - Transfer Accumulator to Stack Pointer
- `TSC` - Transfer Stack Pointer to Accumulator

**Stack push effective address:**
- `PEA $addr` - Push Effective Absolute Address
- `PEI ($dp)` - Push Effective Indirect Address
- `PER label` - Push Effective PC Relative Address

**Test and modify bits:**
- `TSB $dp`, `TSB $addr` - Test and Set Bits (Memory |= A)
- `TRB $dp`, `TRB $addr` - Test and Reset Bits (Memory &= ~A)

**Software interrupts:**
- `BRK #sig` - Software Break
- `COP #sig` - Co-processor Enable

### 8/16-bit Mode Selection (Compile-Time Feature Flags)
The W65816 supports 8-bit or 16-bit width for accumulator (M flag) and index registers (X flag)
independently. This is now supported via compile-time feature flags:

**Usage:**
```bash
llc -march=w65816 test.ll                    # 16-bit mode (default, M=0, X=0)
llc -march=w65816 -mattr=+acc8bit test.ll    # 8-bit accumulator (M=1)
llc -march=w65816 -mattr=+idx8bit test.ll    # 8-bit index registers (X=1)
llc -march=w65816 -mattr=+acc8bit,+idx8bit   # Both 8-bit (limited support)
```

**Processor variants:**
- `w65816` - Default 16-bit mode
- `w65816-m8` - 8-bit accumulator, 16-bit index
- `w65816-x8` - 16-bit accumulator, 8-bit index
- `w65816-mx8` - Both 8-bit

**How it works:**
- Functions emit `SEP #$20` (M=1) or `SEP #$10` (X=1) in prologue to set hardware mode
- The compiler selects appropriate register classes and instruction patterns
- Type legalization promotes i8 to i16 in 16-bit mode; i8 is legal in 8-bit accumulator mode

**Limitations:**
- The combination of +acc8bit,+idx8bit has limited support for i16 values
  (there would be no 16-bit registers available)
- Runtime mode switching is NOT supported - the mode is fixed at compile time
- For temporary mode switches, use inline assembly with SEP/REP instructions
- Common SNES patterns like 8-bit accumulator + 16-bit index work well

### 8-bit (i8) Type Support
**Working:**
- i8 loads from absolute addresses (zextloadi8/sextloadi8 → LDA8_abs)
- i8 stores to absolute addresses (truncstorei8 → STA8_abs)
- i8 arithmetic (add, sub) - promotes to i16
- i8 loads through pointers (`load i8, ptr %p`) - uses LDA8indirect pseudo
- i8 stores through pointers (`store i8 %v, ptr %p`) - uses STA8indirect pseudo

The indirect 8-bit operations use SEP/REP mode switching with stack-relative indirect addressing:
```asm
; 8-bit load through pointer (load i8, ptr %p):
sta 1,s           ; store pointer to stack
sep #32           ; switch to 8-bit mode
ldy #0            ; index = 0
lda (1,s),y       ; load through pointer
rep #32           ; switch back to 16-bit mode
and #255          ; zero-extend result

; 8-bit store through pointer (store i8 %v, ptr %p):
sta 1,s           ; store pointer to stack
txa               ; move value to A (if needed)
sep #32           ; switch to 8-bit mode
ldy #0            ; index = 0
sta (1,s),y       ; store through pointer
rep #32           ; switch back to 16-bit mode
```

- Indexed 8-bit array access (`ptr[i]` for byte arrays) - uses LDA8indirectIdx/STA8indirectIdx pseudos

The indexed 8-bit operations combine pointer indirect addressing with variable index:
```asm
; 8-bit indexed load (load i8, ptr + idx):
pha               ; save A for prologue
sta 1,s           ; store pointer to stack
txy               ; move index to Y
sep #32           ; switch to 8-bit mode
lda (1,s),y       ; load through pointer + index
rep #32           ; switch back to 16-bit mode
and #255          ; zero-extend result

; 8-bit indexed store (store i8 val, ptr + idx):
phy               ; save value (in Y) to stack
sta 1,s           ; store pointer to stack
txy               ; move index (in X) to Y
pla               ; pull saved value to A
sep #32           ; switch to 8-bit mode
sta (1,s),y       ; store through pointer + index
rep #32           ; switch back to 16-bit mode
```

**All 8-bit indirect operations now working.**

### Addressing Modes
**Working:**
- Absolute addressing: `lda addr`, `sta addr`
- Constant offset: `lda addr+6` (address folding)
- Stack-relative: `lda 1,s`, `sta 1,s`
- Indexed absolute X: `lda addr,x`, `sta addr,x` (for variable array access)
- Indexed absolute Y: `lda addr,y`, `sta addr,y` (selected when index is in Y)
- Direct page addressing: `lda dp`, `sta dp` (for globals in `.zeropage` section)
- Indexed direct page X: `lda dp,x`, `sta dp,x` (for zero page arrays with X index)
- **Long (24-bit) addressing**: `lda $123456`, `sta $123456` (for globals in `.fardata`, `.rodata`, `.romdata` sections)

**Long Addressing Details:**
Globals in the following sections automatically use 24-bit (4-byte) long addressing:
- `.fardata` - far data in other banks
- `.rodata` - read-only data (ROM)
- `.romdata` - explicit ROM data
- `.bank*` - any section starting with `.bank`

This enables cross-bank access for SNES development where ROM data is in banks $00-$7F.

**Long indexed (implemented):**
- `lda $123456,x` - **has selection pattern** for far global arrays with variable index
- `sta $123456,x` - **has selection pattern** for far global arrays with variable index

**Working (implemented for pointer dereference):**
- Stack-relative indirect: `lda (n,s),y` / `sta (n,s),y` - used for pointer dereference
  - Pointers in registers are stored to stack slot, then accessed via (offset,S),Y
  - Supports both 16-bit and 8-bit loads/stores through pointers

**Defined but no selection patterns:**
- DP indirect: `lda ($dp)` / `sta ($dp),y` - direct page indirect modes
- DP indirect long: `lda [$dp]` (LDA_dpIndLong defined)
- DP indirect long indexed: `lda [$dp],y` (LDA_dpIndLongY defined)

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
1. ~~8-bit mode support~~ (DONE - compile-time feature flags +acc8bit, +idx8bit)
2. ~~Long (24-bit) addressing~~ (DONE - for globals in .fardata, .rodata, .romdata)
3. ~~Interrupt handling~~ (DONE - use `__attribute__((interrupt))`)

### Interrupt Handling
Functions can be marked as interrupt handlers using the `interrupt` attribute:
```c
__attribute__((interrupt))
void irq_handler(void) {
    // Handle interrupt
}
```

Interrupt handlers have special prologue/epilogue:
- **Prologue**: `rep #48` (ensure 16-bit mode), `pha`, `phx`, `phy` (save all registers)
- **Epilogue**: `rep #48`, `ply`, `plx`, `pla` (restore registers), `rti` (return from interrupt)

Note: The 65816 hardware automatically saves the processor status (P) and program counter (PC) on interrupt entry. The prologue saves A, X, Y which the compiler may use.

## Test Cases That Fail

### Loops with Conditional Branches ✅ FIXED
```llvm
; Now works! Loops with icmp and br i1 compile correctly
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

**Fix:** The issue was that condition codes were being created as MVT::i8 constants,
but i8 is not a legal type in the backend (only i16 is registered). Changed all
condition code constants to use MVT::i16.

**Implementation:**
- LowerSETCC: Returns 0 or 1 in i16 via SELECT_CC
- LowerBRCOND: Compares condition with zero and branches if non-zero
- LowerBR_CC: Custom lowering for combined comparison+branch
- setBooleanContents(ZeroOrOneBooleanContent) configured
- All condition code constants use MVT::i16

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
