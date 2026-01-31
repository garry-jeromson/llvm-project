;===============================================================================
; W65816 Runtime Library Test Suite
;===============================================================================
; Self-contained test that verifies the runtime math functions.
;
; After execution:
;   $0000 = Total tests run
;   $0002 = Tests passed
;   $0004 = Tests failed
;   $0006 = 0x600D if all passed, 0xFAIL (0xFA11) if any failed
;
; The test ends with an infinite loop. Check memory at $0000-$0007 for results.
;
; Build with ca65:
;   ca65 --cpu 65816 -o test_runtime.o test_runtime.s
;   ca65 --cpu 65816 -o w65816_runtime.o w65816_runtime.s
;   ld65 -o test_runtime.bin test_runtime.o w65816_runtime.o -C test_runtime.cfg
;
; Or assemble as a flat binary starting at your desired address.
;===============================================================================

.p816
.smart
.a16
.i16

; Import runtime functions
.import __mulhi3
.import __divhi3
.import __udivhi3
.import __modhi3
.import __umodhi3
.import memcpy
.import memset
.import memmove

;-------------------------------------------------------------------------------
; Test result storage (in zero page for easy inspection)
;-------------------------------------------------------------------------------
.zeropage
test_count:     .res 2          ; $0000: Total tests run
test_passed:    .res 2          ; $0002: Tests passed
test_failed:    .res 2          ; $0004: Tests failed
test_status:    .res 2          ; $0006: 0x600D = all pass, 0xFA11 = fail

; Temporary for test cases
expected:       .res 2          ; $0008: Expected result
actual:         .res 2          ; $000A: Actual result

;-------------------------------------------------------------------------------
; Test buffers for memory operations (in BSS)
;-------------------------------------------------------------------------------
.bss
src_buf:        .res 32         ; Source buffer for memory tests
dest_buf:       .res 32         ; Destination buffer for memory tests
overlap_buf:    .res 32         ; Buffer for overlap tests

;-------------------------------------------------------------------------------
; Code
;-------------------------------------------------------------------------------
.code

.proc reset
        clc
        xce                     ; Enter native mode
        rep #$30                ; 16-bit A and X/Y
        .a16
        .i16

        ; Initialize counters
        stz test_count
        stz test_passed
        stz test_failed

        ; Run all tests
        jsr test_mulhi3
        jsr test_udivhi3
        jsr test_divhi3
        jsr test_umodhi3
        jsr test_modhi3
        jsr test_memcpy
        jsr test_memset
        jsr test_memmove

        ; Set final status
        lda test_failed
        bne @failed
        lda #$600D              ; "GOOD" - all tests passed
        bra @done
@failed:
        lda #$FA11              ; "FAIL" - some tests failed
@done:
        sta test_status

        ; Stop CPU - test runner will detect this
        stp                     ; STP instruction (0xDB) halts the CPU
.endproc

;===============================================================================
; Test helper: check_result
; Input: A = actual result, expected in 'expected' variable
; Increments pass or fail counter
;===============================================================================
.proc check_result
        sta actual
        inc test_count

        cmp expected
        bne @fail

        inc test_passed
        rts

@fail:
        inc test_failed
        rts
.endproc

;===============================================================================
; __mulhi3 tests
;===============================================================================
.proc test_mulhi3
        ; Test 1: 0 * 0 = 0
        lda #0
        sta expected
        lda #0
        ldx #0
        jsr __mulhi3
        jsr check_result

        ; Test 2: 1 * 1 = 1
        lda #1
        sta expected
        lda #1
        ldx #1
        jsr __mulhi3
        jsr check_result

        ; Test 3: 5 * 7 = 35
        lda #35
        sta expected
        lda #5
        ldx #7
        jsr __mulhi3
        jsr check_result

        ; Test 4: 7 * 5 = 35 (commutative)
        lda #35
        sta expected
        lda #7
        ldx #5
        jsr __mulhi3
        jsr check_result

        ; Test 5: 100 * 100 = 10000
        lda #10000
        sta expected
        lda #100
        ldx #100
        jsr __mulhi3
        jsr check_result

        ; Test 6: 256 * 256 = 0 (overflow, low 16 bits)
        lda #0
        sta expected
        lda #256
        ldx #256
        jsr __mulhi3
        jsr check_result

        ; Test 7: 255 * 255 = 65025
        lda #65025
        sta expected
        lda #255
        ldx #255
        jsr __mulhi3
        jsr check_result

        ; Test 8: 1000 * 50 = 50000
        lda #50000
        sta expected
        lda #1000
        ldx #50
        jsr __mulhi3
        jsr check_result

        rts
.endproc

;===============================================================================
; __udivhi3 tests (unsigned division)
;===============================================================================
.proc test_udivhi3
        ; Test 1: 0 / 1 = 0
        lda #0
        sta expected
        lda #0
        ldx #1
        jsr __udivhi3
        jsr check_result

        ; Test 2: 10 / 2 = 5
        lda #5
        sta expected
        lda #10
        ldx #2
        jsr __udivhi3
        jsr check_result

        ; Test 3: 100 / 10 = 10
        lda #10
        sta expected
        lda #100
        ldx #10
        jsr __udivhi3
        jsr check_result

        ; Test 4: 7 / 3 = 2 (truncation)
        lda #2
        sta expected
        lda #7
        ldx #3
        jsr __udivhi3
        jsr check_result

        ; Test 5: 65535 / 1 = 65535
        lda #65535
        sta expected
        lda #65535
        ldx #1
        jsr __udivhi3
        jsr check_result

        ; Test 6: 65535 / 65535 = 1
        lda #1
        sta expected
        lda #65535
        ldx #65535
        jsr __udivhi3
        jsr check_result

        ; Test 7: 1000 / 7 = 142
        lda #142
        sta expected
        lda #1000
        ldx #7
        jsr __udivhi3
        jsr check_result

        ; Test 8: 50000 / 1000 = 50
        lda #50
        sta expected
        lda #50000
        ldx #1000
        jsr __udivhi3
        jsr check_result

        rts
.endproc

;===============================================================================
; __divhi3 tests (signed division)
;===============================================================================
.proc test_divhi3
        ; Test 1: 10 / 2 = 5
        lda #5
        sta expected
        lda #10
        ldx #2
        jsr __divhi3
        jsr check_result

        ; Test 2: -10 / 2 = -5
        lda #.loword(-5)        ; -5 = 0xFFFB
        sta expected
        lda #.loword(-10)       ; -10 = 0xFFF6
        ldx #2
        jsr __divhi3
        jsr check_result

        ; Test 3: 10 / -2 = -5
        lda #.loword(-5)
        sta expected
        lda #10
        ldx #.loword(-2)        ; -2 = 0xFFFE
        jsr __divhi3
        jsr check_result

        ; Test 4: -10 / -2 = 5
        lda #5
        sta expected
        lda #.loword(-10)
        ldx #.loword(-2)
        jsr __divhi3
        jsr check_result

        ; Test 5: 7 / 3 = 2 (truncation toward zero)
        lda #2
        sta expected
        lda #7
        ldx #3
        jsr __divhi3
        jsr check_result

        ; Test 6: -7 / 3 = -2 (truncation toward zero)
        lda #.loword(-2)
        sta expected
        lda #.loword(-7)
        ldx #3
        jsr __divhi3
        jsr check_result

        ; Test 7: 32767 / 1 = 32767 (max positive)
        lda #32767
        sta expected
        lda #32767
        ldx #1
        jsr __divhi3
        jsr check_result

        ; Test 8: -32768 / 1 = -32768 (min negative)
        lda #.loword(-32768)    ; 0x8000
        sta expected
        lda #.loword(-32768)
        ldx #1
        jsr __divhi3
        jsr check_result

        rts
.endproc

;===============================================================================
; __umodhi3 tests (unsigned remainder)
;===============================================================================
.proc test_umodhi3
        ; Test 1: 0 % 1 = 0
        lda #0
        sta expected
        lda #0
        ldx #1
        jsr __umodhi3
        jsr check_result

        ; Test 2: 10 % 3 = 1
        lda #1
        sta expected
        lda #10
        ldx #3
        jsr __umodhi3
        jsr check_result

        ; Test 3: 100 % 10 = 0
        lda #0
        sta expected
        lda #100
        ldx #10
        jsr __umodhi3
        jsr check_result

        ; Test 4: 7 % 3 = 1
        lda #1
        sta expected
        lda #7
        ldx #3
        jsr __umodhi3
        jsr check_result

        ; Test 5: 65535 % 1000 = 535
        lda #535
        sta expected
        lda #65535
        ldx #1000
        jsr __umodhi3
        jsr check_result

        ; Test 6: 1000 % 7 = 6
        lda #6
        sta expected
        lda #1000
        ldx #7
        jsr __umodhi3
        jsr check_result

        ; Test 7: 255 % 16 = 15
        lda #15
        sta expected
        lda #255
        ldx #16
        jsr __umodhi3
        jsr check_result

        ; Test 8: 50001 % 1000 = 1
        lda #1
        sta expected
        lda #50001
        ldx #1000
        jsr __umodhi3
        jsr check_result

        rts
.endproc

;===============================================================================
; __modhi3 tests (signed remainder)
;===============================================================================
.proc test_modhi3
        ; Test 1: 10 % 3 = 1
        lda #1
        sta expected
        lda #10
        ldx #3
        jsr __modhi3
        jsr check_result

        ; Test 2: -10 % 3 = -1 (sign matches dividend)
        lda #.loword(-1)        ; -1 = 0xFFFF
        sta expected
        lda #.loword(-10)
        ldx #3
        jsr __modhi3
        jsr check_result

        ; Test 3: 10 % -3 = 1 (sign matches dividend)
        lda #1
        sta expected
        lda #10
        ldx #.loword(-3)
        jsr __modhi3
        jsr check_result

        ; Test 4: -10 % -3 = -1 (sign matches dividend)
        lda #.loword(-1)
        sta expected
        lda #.loword(-10)
        ldx #.loword(-3)
        jsr __modhi3
        jsr check_result

        ; Test 5: 7 % 4 = 3
        lda #3
        sta expected
        lda #7
        ldx #4
        jsr __modhi3
        jsr check_result

        ; Test 6: -7 % 4 = -3
        lda #.loword(-3)
        sta expected
        lda #.loword(-7)
        ldx #4
        jsr __modhi3
        jsr check_result

        ; Test 7: 100 % 100 = 0
        lda #0
        sta expected
        lda #100
        ldx #100
        jsr __modhi3
        jsr check_result

        ; Test 8: -32768 % 3 = -2
        lda #.loword(-2)
        sta expected
        lda #.loword(-32768)
        ldx #3
        jsr __modhi3
        jsr check_result

        rts
.endproc

;===============================================================================
; memcpy tests
;===============================================================================
.proc test_memcpy
        ; Test 1: Copy 0 bytes (should not crash)
        lda #.loword(dest_buf)
        ldx #.loword(src_buf)
        ldy #0
        jsr memcpy
        ; Just verify it returns dest
        cmp #.loword(dest_buf)
        bne @fail1
        inc test_count
        inc test_passed
        bra @test2
@fail1:
        inc test_count
        inc test_failed

@test2:
        ; Test 2: Copy 4 bytes
        ; First, initialize source buffer with known values
        sep #$20
.a8
        lda #$11
        sta src_buf
        lda #$22
        sta src_buf+1
        lda #$33
        sta src_buf+2
        lda #$44
        sta src_buf+3
        ; Clear dest
        lda #0
        sta dest_buf
        sta dest_buf+1
        sta dest_buf+2
        sta dest_buf+3
        rep #$20
.a16
        ; Do the copy
        lda #.loword(dest_buf)
        ldx #.loword(src_buf)
        ldy #4
        jsr memcpy
        ; Verify
        sep #$20
.a8
        lda dest_buf
        cmp #$11
        bne @fail2
        lda dest_buf+1
        cmp #$22
        bne @fail2
        lda dest_buf+2
        cmp #$33
        bne @fail2
        lda dest_buf+3
        cmp #$44
        bne @fail2
        rep #$20
.a16
        inc test_count
        inc test_passed
        bra @test3
@fail2:
        rep #$20
.a16
        inc test_count
        inc test_failed

@test3:
        ; Test 3: Copy 1 byte
        sep #$20
.a8
        lda #$AA
        sta src_buf
        lda #0
        sta dest_buf
        rep #$20
.a16
        lda #.loword(dest_buf)
        ldx #.loword(src_buf)
        ldy #1
        jsr memcpy
        sep #$20
.a8
        lda dest_buf
        cmp #$AA
        bne @fail3
        rep #$20
.a16
        inc test_count
        inc test_passed
        rts
@fail3:
        rep #$20
.a16
        inc test_count
        inc test_failed
        rts
.endproc

;===============================================================================
; memset tests
;===============================================================================
.proc test_memset
        ; Test 1: Set 0 bytes (should not crash)
        lda #.loword(dest_buf)
        ldx #$FF
        ldy #0
        jsr memset
        ; Just verify it returns dest
        cmp #.loword(dest_buf)
        bne @fail1
        inc test_count
        inc test_passed
        bra @test2
@fail1:
        inc test_count
        inc test_failed

@test2:
        ; Test 2: Set 4 bytes to $55
        ; First clear the buffer
        sep #$20
.a8
        lda #0
        sta dest_buf
        sta dest_buf+1
        sta dest_buf+2
        sta dest_buf+3
        rep #$20
.a16
        ; Do the memset
        lda #.loword(dest_buf)
        ldx #$55
        ldy #4
        jsr memset
        ; Verify
        sep #$20
.a8
        lda dest_buf
        cmp #$55
        bne @fail2
        lda dest_buf+1
        cmp #$55
        bne @fail2
        lda dest_buf+2
        cmp #$55
        bne @fail2
        lda dest_buf+3
        cmp #$55
        bne @fail2
        rep #$20
.a16
        inc test_count
        inc test_passed
        bra @test3
@fail2:
        rep #$20
.a16
        inc test_count
        inc test_failed

@test3:
        ; Test 3: Set 1 byte to $00 (zero fill)
        sep #$20
.a8
        lda #$FF
        sta dest_buf
        rep #$20
.a16
        lda #.loword(dest_buf)
        ldx #0
        ldy #1
        jsr memset
        sep #$20
.a8
        lda dest_buf
        cmp #0
        bne @fail3
        rep #$20
.a16
        inc test_count
        inc test_passed
        rts
@fail3:
        rep #$20
.a16
        inc test_count
        inc test_failed
        rts
.endproc

;===============================================================================
; memmove tests
;===============================================================================
.proc test_memmove
        ; Test 1: Non-overlapping copy (same as memcpy)
        sep #$20
.a8
        lda #$AA
        sta src_buf
        lda #$BB
        sta src_buf+1
        lda #0
        sta dest_buf
        sta dest_buf+1
        rep #$20
.a16
        lda #.loword(dest_buf)
        ldx #.loword(src_buf)
        ldy #2
        jsr memmove
        sep #$20
.a8
        lda dest_buf
        cmp #$AA
        bne @fail1
        lda dest_buf+1
        cmp #$BB
        bne @fail1
        rep #$20
.a16
        inc test_count
        inc test_passed
        bra @test2
@fail1:
        rep #$20
.a16
        inc test_count
        inc test_failed

@test2:
        ; Test 2: Overlapping copy (dest > src) - backward copy needed
        ; Set up overlap_buf: [1,2,3,4,5,0,0,0]
        ; Copy bytes 0-4 to bytes 2-6: result should be [1,2,1,2,3,4,5,0]
        sep #$20
.a8
        lda #1
        sta overlap_buf
        lda #2
        sta overlap_buf+1
        lda #3
        sta overlap_buf+2
        lda #4
        sta overlap_buf+3
        lda #5
        sta overlap_buf+4
        lda #0
        sta overlap_buf+5
        sta overlap_buf+6
        sta overlap_buf+7
        rep #$20
.a16
        ; memmove(overlap_buf+2, overlap_buf, 5)
        lda #.loword(overlap_buf+2)     ; dest
        ldx #.loword(overlap_buf)       ; src
        ldy #5                          ; count
        jsr memmove
        ; Verify: overlap_buf should be [1,2,1,2,3,4,5,0]
        sep #$20
.a8
        lda overlap_buf
        cmp #1
        bne @fail2
        lda overlap_buf+1
        cmp #2
        bne @fail2
        lda overlap_buf+2
        cmp #1
        bne @fail2
        lda overlap_buf+3
        cmp #2
        bne @fail2
        lda overlap_buf+4
        cmp #3
        bne @fail2
        lda overlap_buf+5
        cmp #4
        bne @fail2
        lda overlap_buf+6
        cmp #5
        bne @fail2
        rep #$20
.a16
        inc test_count
        inc test_passed
        bra @test3
@fail2:
        rep #$20
.a16
        inc test_count
        inc test_failed

@test3:
        ; Test 3: Move 0 bytes (should not crash, dest == src case)
        lda #.loword(overlap_buf)
        ldx #.loword(overlap_buf)
        ldy #0
        jsr memmove
        ; Just verify it returns (doesn't crash)
        inc test_count
        inc test_passed
        rts
.endproc

;===============================================================================
; Vectors (for systems that use hardware vectors)
;===============================================================================
.segment "VECTORS"
        .word 0                 ; $FFFA: NMI vector (unused)
        .word reset             ; $FFFC: Reset vector
        .word 0                 ; $FFFE: IRQ vector (unused)
