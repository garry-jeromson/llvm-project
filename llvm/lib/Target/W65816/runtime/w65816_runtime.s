;===============================================================================
; W65816 Runtime Library
;===============================================================================
; Provides runtime support functions for the W65816 LLVM backend.
;
; Calling convention:
;   - First argument:  A register (16-bit)
;   - Second argument: X register (16-bit)
;   - Return value:    A register (16-bit)
;
; All functions preserve the direct page register (D) and data bank register (DBR).
; Functions may clobber A, X, Y, and the processor flags.
;
; Assemble with: ca65 -o w65816_runtime.o w65816_runtime.s
; Or use any 65816-compatible assembler in 16-bit native mode.
;===============================================================================

.p816                           ; Enable 65816 mode
.smart                          ; Smart mode for accumulator/index sizing
.a16                            ; 16-bit accumulator
.i16                            ; 16-bit index registers

.export __mulhi3
.export __divhi3
.export __udivhi3
.export __modhi3
.export __umodhi3
.export memcpy
.export memset
.export memmove

;-------------------------------------------------------------------------------
; Temporary storage
;-------------------------------------------------------------------------------
; Absolute addresses (for arithmetic temporaries)
.bss
_tmp0:      .res 2              ; Temporary storage
_tmp1:      .res 2              ; Temporary storage
_tmp2:      .res 2              ; Temporary storage (for sign handling)

; Zero page addresses (required for indirect addressing in memcpy/memmove/memset)
; Use fixed high ZP addresses ($F0-$F5) to avoid conflicts with user ZP usage
_zp_ptr0 = $F0                  ; Zero page pointer 0 ($F0-$F1)
_zp_ptr1 = $F2                  ; Zero page pointer 1 ($F2-$F3)
_zp_tmp  = $F4                  ; Zero page temporary ($F4-$F5)

.code

;===============================================================================
; __mulhi3 - Unsigned 16-bit multiplication
;===============================================================================
; Input:  A = multiplicand, X = multiplier
; Output: A = low 16 bits of product (A * X)
; Clobbers: X, Y
;
; Algorithm: Shift-and-add multiplication
; For each bit in multiplier: if bit is set, add multiplicand to result
; Then shift multiplicand left and multiplier right
;===============================================================================
.proc __mulhi3
        sta _tmp0               ; Save multiplicand
        stx _tmp1               ; Save multiplier
        lda #0                  ; Initialize result to 0
        ldy #16                 ; 16 bits to process

@loop:
        lsr _tmp1               ; Shift multiplier right, low bit -> carry
        bcc @skip               ; If bit was 0, skip addition
        clc
        adc _tmp0               ; Add multiplicand to result
@skip:
        asl _tmp0               ; Shift multiplicand left
        dey
        bne @loop

        rts                     ; Return with result in A
.endproc

;===============================================================================
; __udivhi3 - Unsigned 16-bit division
;===============================================================================
; Input:  A = dividend, X = divisor
; Output: A = quotient (A / X)
; Clobbers: X, Y
;
; Algorithm: Binary long division (shift-and-subtract)
;===============================================================================
.proc __udivhi3
        cpx #0                  ; Check for divide by zero
        beq @div_zero

        sta _tmp0               ; dividend (will become quotient)
        stx _tmp1               ; divisor
        lda #0                  ; remainder
        ldy #16                 ; 16 bits to process

@loop:
        asl _tmp0               ; Shift dividend left, high bit -> carry
        rol a                   ; Shift carry into remainder

        cmp _tmp1               ; Compare remainder with divisor
        bcc @skip               ; If remainder < divisor, skip subtraction

        sbc _tmp1               ; Subtract divisor from remainder
        inc _tmp0               ; Set low bit of quotient
@skip:
        dey
        bne @loop

        lda _tmp0               ; Return quotient in A
        rts

@div_zero:
        lda #$FFFF              ; Return -1 (0xFFFF) for divide by zero
        rts
.endproc

;===============================================================================
; __divhi3 - Signed 16-bit division
;===============================================================================
; Input:  A = dividend, X = divisor
; Output: A = quotient (A / X), signed
; Clobbers: X, Y
;
; Algorithm: Convert to unsigned, divide, then fix sign
; Sign of result = sign of dividend XOR sign of divisor
;===============================================================================
.proc __divhi3
        stz _tmp2               ; Clear sign flag (0 = positive result)

        ; Check dividend sign
        cmp #$8000              ; Check if negative (bit 15 set)
        bcc @dividend_pos
        eor #$FFFF              ; Negate: invert all bits
        inc a                   ; Add 1 to complete two's complement
        inc _tmp2               ; Toggle sign flag
@dividend_pos:

        sta _tmp0               ; Save absolute dividend

        ; Check divisor sign
        txa
        cmp #$8000
        bcc @divisor_pos
        eor #$FFFF              ; Negate divisor
        inc a
        inc _tmp2               ; Toggle sign flag
@divisor_pos:
        tax                     ; Absolute divisor in X

        lda _tmp0               ; Absolute dividend in A
        jsr __udivhi3           ; Unsigned divide

        ; Check if result should be negative
        lsr _tmp2               ; Shift sign flag to carry
        bcc @done               ; If even number of negatives, result is positive

        eor #$FFFF              ; Negate result
        inc a
@done:
        rts
.endproc

;===============================================================================
; __umodhi3 - Unsigned 16-bit remainder
;===============================================================================
; Input:  A = dividend, X = divisor
; Output: A = remainder (A % X)
; Clobbers: X, Y
;
; Algorithm: Same as division, but return remainder instead of quotient
;===============================================================================
.proc __umodhi3
        cpx #0                  ; Check for divide by zero
        beq @mod_zero

        sta _tmp0               ; dividend
        stx _tmp1               ; divisor
        lda #0                  ; remainder
        ldy #16                 ; 16 bits to process

@loop:
        asl _tmp0               ; Shift dividend left
        rol a                   ; Shift into remainder

        cmp _tmp1               ; Compare remainder with divisor
        bcc @skip
        sbc _tmp1               ; Subtract divisor
        inc _tmp0               ; Set quotient bit (not used but keeps algorithm correct)
@skip:
        dey
        bne @loop

        rts                     ; Return remainder in A

@mod_zero:
        lda #0                  ; Return 0 for mod by zero
        rts
.endproc

;===============================================================================
; __modhi3 - Signed 16-bit remainder
;===============================================================================
; Input:  A = dividend, X = divisor
; Output: A = remainder (A % X), signed
; Clobbers: X, Y
;
; Note: Sign of remainder matches sign of dividend (C99 behavior)
;===============================================================================
.proc __modhi3
        stz _tmp2               ; Clear sign flag

        ; Check dividend sign (remainder has same sign as dividend)
        cmp #$8000
        bcc @dividend_pos
        eor #$FFFF
        inc a
        inc _tmp2               ; Remember dividend was negative
@dividend_pos:

        sta _tmp0               ; Save absolute dividend

        ; Get absolute value of divisor
        txa
        cmp #$8000
        bcc @divisor_pos
        eor #$FFFF
        inc a
@divisor_pos:
        tax

        lda _tmp0
        jsr __umodhi3           ; Unsigned mod

        ; Apply sign of original dividend to remainder
        lsr _tmp2
        bcc @done
        eor #$FFFF
        inc a
@done:
        rts
.endproc

;===============================================================================
; memcpy - Copy memory block
;===============================================================================
; Input:  A = dest pointer, X = src pointer, Y = byte count
; Output: A = dest pointer (unchanged)
; Clobbers: X, Y
;
; Copies Y bytes from src to dest. Does not handle overlapping regions
; (use memmove for that). For overlapping regions where dest > src,
; this will produce incorrect results.
;===============================================================================
.proc memcpy
        sta _zp_tmp             ; Save dest for return value
        sta _zp_ptr0            ; dest pointer (in ZP for indirect addressing)
        stx _zp_ptr1            ; src pointer (in ZP for indirect addressing)

        cpy #0                  ; Check if count is 0
        beq @done

        ; Use 8-bit accumulator for byte copying
        sep #$20                ; 8-bit accumulator
.a8
@loop:
        lda (_zp_ptr1)          ; Load byte from src
        sta (_zp_ptr0)          ; Store byte to dest

        ; Increment pointers
        rep #$20                ; 16-bit accumulator for pointer math
.a16
        inc _zp_ptr0
        inc _zp_ptr1

        dey                     ; Decrement count
        bne @loop_continue
        bra @done

@loop_continue:
        sep #$20                ; Back to 8-bit for next iteration
.a8
        bra @loop

@done:
        rep #$20                ; Ensure 16-bit mode on exit
.a16
        lda _zp_tmp             ; Return original dest
        rts
.endproc

;===============================================================================
; memset - Fill memory with a byte value
;===============================================================================
; Input:  A = dest pointer, X = fill value (low byte used), Y = byte count
; Output: A = dest pointer (unchanged)
; Clobbers: X, Y
;
; Fills Y bytes starting at dest with the low byte of X.
;===============================================================================
.proc memset
        sta _zp_tmp             ; Save dest for return value
        sta _zp_ptr0            ; dest pointer (in ZP for indirect addressing)
        stx _tmp0               ; Save fill value

        cpy #0                  ; Check if count is 0
        beq @done

        ; Use 8-bit accumulator for byte operations
        sep #$20                ; 8-bit accumulator
.a8
        lda _tmp0               ; Get fill value (low byte)

@loop:
        sta (_zp_ptr0)          ; Store byte to dest

        ; Increment pointer (need 16-bit mode)
        rep #$20
.a16
        inc _zp_ptr0
        sep #$20
.a8

        dey                     ; Decrement count
        bne @loop

@done:
        rep #$20                ; Ensure 16-bit mode on exit
.a16
        lda _zp_tmp             ; Return original dest
        rts
.endproc

;===============================================================================
; memmove - Copy memory block (handles overlapping regions)
;===============================================================================
; Input:  A = dest pointer, X = src pointer, Y = byte count
; Output: A = dest pointer (unchanged)
; Clobbers: X, Y
;
; Copies Y bytes from src to dest. Correctly handles overlapping regions
; by copying forward if dest < src, backward if dest > src.
;===============================================================================
.proc memmove
        sta _zp_tmp             ; Save dest for return value

        cpy #0                  ; Check if count is 0
        beq @done_early

        ; Compare dest and src to determine copy direction
        ; If dest <= src, copy forward (safe)
        ; If dest > src, copy backward (handles overlap)
        stx _tmp0               ; src (temporary)
        cmp _tmp0               ; Compare dest with src
        bcc @forward            ; dest < src: copy forward
        beq @done_early         ; dest == src: nothing to do
        bra @backward           ; dest > src: copy backward

@forward:
        ; Copy forward (low to high addresses)
        sta _zp_ptr0            ; dest pointer (ZP for indirect)
        stx _zp_ptr1            ; src pointer (ZP for indirect)

        sep #$20                ; 8-bit accumulator
.a8
@fwd_loop:
        lda (_zp_ptr1)          ; Load byte from src
        sta (_zp_ptr0)          ; Store byte to dest

        rep #$20                ; 16-bit for pointer math
.a16
        inc _zp_ptr0
        inc _zp_ptr1
        sep #$20                ; Back to 8-bit
.a8

        dey
        bne @fwd_loop
        bra @done

@backward:
        ; Copy backward (high to low addresses)
        ; Start from dest + count - 1 and src + count - 1
        sta _zp_ptr0            ; dest
        stx _zp_ptr1            ; src

        ; Add count-1 to both pointers
        rep #$20
.a16
        tya                     ; count
        dec a                   ; count - 1
        clc
        adc _zp_ptr0
        sta _zp_ptr0            ; dest + count - 1
        tya
        dec a
        clc
        adc _zp_ptr1
        sta _zp_ptr1            ; src + count - 1

        sep #$20                ; 8-bit accumulator
.a8
@bwd_loop:
        lda (_zp_ptr1)          ; Load byte from src
        sta (_zp_ptr0)          ; Store byte to dest

        rep #$20                ; 16-bit for pointer math
.a16
        dec _zp_ptr0
        dec _zp_ptr1
        sep #$20                ; Back to 8-bit
.a8

        dey
        bne @bwd_loop

@done:
        rep #$20                ; Ensure 16-bit mode on exit
.a16
        lda _zp_tmp             ; Return original dest
        rts

@done_early:
        rts
.endproc
