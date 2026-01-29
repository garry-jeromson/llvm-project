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

;-------------------------------------------------------------------------------
; Temporary storage (in direct page or absolute - adjust as needed)
; These can be moved to zero page for better performance
;-------------------------------------------------------------------------------
.bss
_tmp0:      .res 2              ; Temporary storage
_tmp1:      .res 2              ; Temporary storage
_tmp2:      .res 2              ; Temporary storage (for sign handling)

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
