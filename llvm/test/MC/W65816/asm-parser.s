; RUN: llvm-mc -triple=w65816 -show-encoding %s | FileCheck %s

;=============================================================================
; Immediate Mode (with # prefix)
;=============================================================================

; CHECK: lda #1234               ; encoding: [0xa9,0xd2,0x04]
  lda #1234

; CHECK: ldx #0                  ; encoding: [0xa2,0x00,0x00]
  ldx #0

; CHECK: ldy #255                ; encoding: [0xa0,0xff,0x00]
  ldy #255

;=============================================================================
; Absolute Addressing
;=============================================================================

; CHECK: lda $2000               ; encoding: [0xad,0x00,0x20]
  lda 8192

; CHECK: sta $2000               ; encoding: [0x8d,0x00,0x20]
  sta 8192

; CHECK: lda $0050               ; encoding: [0xad,0x50,0x00]
  lda 80

;=============================================================================
; Accumulator Mode
;=============================================================================

; CHECK: asl a                   ; encoding: [0x0a]
  asl a

; CHECK: lsr a                   ; encoding: [0x4a]
  lsr a

; CHECK: rol a                   ; encoding: [0x2a]
  rol a

; CHECK: ror a                   ; encoding: [0x6a]
  ror a

; CHECK: inc a                   ; encoding: [0x1a]
  inc a

; CHECK: dec a                   ; encoding: [0x3a]
  dec a

;=============================================================================
; Stack Operations
;=============================================================================

; CHECK: pha                     ; encoding: [0x48]
  pha

; CHECK: pla                     ; encoding: [0x68]
  pla

; CHECK: phx                     ; encoding: [0xda]
  phx

; CHECK: plx                     ; encoding: [0xfa]
  plx

; CHECK: phy                     ; encoding: [0x5a]
  phy

; CHECK: ply                     ; encoding: [0x7a]
  ply

;=============================================================================
; Control Flow
;=============================================================================

; CHECK: jsr $1000               ; encoding: [0x20,0x00,0x10]
  jsr 4096

; CHECK: rts                     ; encoding: [0x60]
  rts

; CHECK: rti                     ; encoding: [0x40]
  rti

;=============================================================================
; Indirect Addressing - Direct Page
;=============================================================================

; CHECK: lda ($50)               ; encoding: [0xb2,0x50]
  lda (0x50)

; CHECK: lda ($50),y             ; encoding: [0xb1,0x50]
  lda (0x50),y

; CHECK: lda ($50,x)             ; encoding: [0xa1,0x50]
  lda (0x50,x)

; CHECK: sta ($60)               ; encoding: [0x92,0x60]
  sta (0x60)

; CHECK: sta ($60),y             ; encoding: [0x91,0x60]
  sta (0x60),y

; CHECK: sta ($60,x)             ; encoding: [0x81,0x60]
  sta (0x60,x)

;=============================================================================
; Indirect Long Addressing
;=============================================================================

; CHECK: lda [$50]               ; encoding: [0xa7,0x50]
  lda [0x50]

; CHECK: lda [$50],y             ; encoding: [0xb7,0x50]
  lda [0x50],y

; CHECK: sta [$60]               ; encoding: [0x87,0x60]
  sta [0x60]

; CHECK: sta [$60],y             ; encoding: [0x97,0x60]
  sta [0x60],y

;=============================================================================
; Stack Relative Indirect Indexed
;=============================================================================

; CHECK: lda ($05,s),y           ; encoding: [0xb3,0x05]
  lda (5,s),y

; CHECK: sta ($03,s),y           ; encoding: [0x93,0x03]
  sta (3,s),y

;=============================================================================
; JMP Indirect Modes
;=============================================================================

; CHECK: jmp ($1000)             ; encoding: [0x6c,0x00,0x10]
  jmp (0x1000)

; CHECK: jmp ($2000,x)           ; encoding: [0x7c,0x00,0x20]
  jmp (0x2000,x)

; CHECK: jmp [$3000]             ; encoding: [0xdc,0x00,0x30]
  jmp [0x3000]
