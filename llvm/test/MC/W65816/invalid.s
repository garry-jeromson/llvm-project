; RUN: not llvm-mc -triple w65816 %s 2>&1 | FileCheck %s

; Test invalid instructions and operands

; CHECK: error: unrecognized instruction mnemonic 'foo'
foo

; CHECK: error: unrecognized instruction mnemonic 'xyz'
xyz 0x1234
