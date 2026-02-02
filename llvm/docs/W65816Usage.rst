==============================
User Guide for W65816 Target
==============================

.. contents::
   :local:

Introduction
============

The W65816 target provides code generation for the Western Design Center
W65816 16-bit microprocessor. This processor was used in the Super Nintendo
Entertainment System (SNES), Apple IIGS, and various embedded systems.

The backend lives in the ``llvm/lib/Target/W65816`` directory.

Target Triple
=============

The W65816 backend uses the following target triple:

.. code-block:: text

   w65816-unknown-none

Example usage with Clang:

.. code-block:: bash

   clang -target w65816-unknown-none -O2 -S example.c -o example.s

Architecture Overview
=====================

The W65816 is a 16-bit extension of the 6502 processor with:

- **Registers**: A (accumulator), X, Y (index), SP (stack pointer), D (direct page), DBR (data bank), PBR (program bank)
- **Data sizes**: 8-bit or 16-bit (mode switchable via M/X processor flags)
- **Address space**: 24-bit (16MB) with bank registers
- **Stack**: 16-bit stack pointer in bank 0

The backend operates in native 16-bit mode by default. Emulation mode (6502 compatibility) is not supported.

Optimization Levels
===================

Due to the W65816's limited register set (only A, X, Y for general use),
optimization is required for successful compilation:

- **-O0**: Explicitly blocked. Will produce an error.
- **-O1**: Minimum required level. Recommended for most code.
- **-O2/-O3**: Fully supported.

When no ``-O`` flag is specified, the toolchain defaults to ``-O1``.

Calling Convention
==================

Arguments
---------

Function arguments are passed as follows:

1. First argument: A register (16-bit)
2. Second argument: X register (16-bit)
3. Third argument: Y register (16-bit)
4. Additional arguments: pushed on stack (right-to-left)

Return Values
-------------

Return values are passed in the A register (16-bit).

Callee-Saved Registers
----------------------

The D (direct page) register is callee-saved when used. A, X, and Y are
caller-saved.

Example
-------

.. code-block:: c

   int add3(int a, int b, int c) {
       return a + b + c;
   }

   // a in A, b in X, c in Y
   // Result returned in A

Data Types
==========

Supported Types
---------------

.. table:: Supported Data Types

   ============  ======  ===========
   C Type        Size    Notes
   ============  ======  ===========
   char          8-bit
   short         16-bit
   int           16-bit  Same as short
   long          32-bit  Software only (no codegen support)
   pointer       16-bit  Bank 0 addressing
   ============  ======  ===========

Unsupported Types
-----------------

32-bit and 64-bit integers are **not supported** in function signatures.
Using ``int32_t`` or ``int64_t`` as arguments or return values will produce
a clear error message:

.. code-block:: text

   error: 32-bit and 64-bit integer arguments are not supported on W65816.
   Use 16-bit types (short, int16_t) instead.

Inline Assembly
===============

Register Constraints
--------------------

The following register constraints are supported:

.. table:: Register Constraints

   ==========  ===========  ================
   Constraint  Register     Description
   ==========  ===========  ================
   a           A            Accumulator
   x           X            X index register
   y           Y            Y index register
   d           D            Direct page register
   s           SP           Stack pointer
   ==========  ===========  ================

Register Aliases
----------------

.. table:: Register Aliases

   ======  =======
   Alias   Register
   ======  =======
   acc     A
   dp      D
   ======  =======

Integer Literals
----------------

W65816 assembly uses Motorola-style integer literals:

- Hexadecimal: ``$FF`` or ``$1234``
- Binary: ``%11110000``
- Decimal: ``255``

Example:

.. code-block:: c

   void set_mode(void) {
       __asm__ volatile("sep #$20");  // 8-bit accumulator
       __asm__ volatile("rep #$30");  // 16-bit A, X, Y
   }

Addressing Modes
================

The W65816 supports 24 addressing modes. Key modes used by the backend:

.. table:: Common Addressing Modes

   =====================  ================  ====================
   Mode                   Syntax            Example
   =====================  ================  ====================
   Immediate              #imm              ``lda #$1234``
   Absolute               addr              ``lda $1234``
   Absolute Indexed X     addr,x            ``lda $1234,x``
   Absolute Indexed Y     addr,y            ``lda $1234,y``
   Direct Page            dp                ``lda $12``
   Stack Relative         offset,s          ``lda 3,s``
   Stack Relative Ind Y   (offset,s),y      ``lda (3,s),y``
   DP Indirect            (dp)              ``lda ($12)``
   DP Indirect Indexed    (dp),y            ``lda ($12),y``
   Long                   long              ``lda $123456``
   Long Indexed X         long,x            ``lda $123456,x``
   =====================  ================  ====================

Function Attributes
===================

Interrupt Handlers
------------------

Use ``__attribute__((interrupt))`` for interrupt service routines:

.. code-block:: c

   __attribute__((interrupt))
   void vblank_handler(void) {
       // Registers A, X, Y saved/restored automatically
       // Returns with RTI instead of RTS
   }

The prologue saves A, X, Y with ``REP #$30; PHA; PHX; PHY`` and the
epilogue restores them before executing ``RTI``.

Far Functions
-------------

Use ``__attribute__((w65816_farfunc))`` for functions called across banks:

.. code-block:: c

   __attribute__((w65816_farfunc))
   int bank1_function(int x) {
       return x + 1;
   }

Far functions are called with ``JSL`` and return with ``RTL`` instead of
``JSR``/``RTS``.

Direct Page Frame
-----------------

Use ``__attribute__((annotate("w65816_dpframe")))`` to allocate local
variables in the 256-byte direct page region instead of on the stack:

.. code-block:: c

   __attribute__((annotate("w65816_dpframe")))
   int fast_function(void) {
       int a, b;  // Allocated in DP, faster access
       return a + b;
   }

**Note**: Local variables must fit in 256 bytes or compilation fails.

Use ``-mattr=+assume-d0`` to skip D register save/restore when D is
guaranteed to be 0 at function entry.

Sections
========

Special section names trigger long (24-bit) addressing:

- ``.fardata`` - Far data
- ``.rodata`` - Read-only data
- ``.romdata`` - ROM data
- ``.bank*`` - Bank-specific sections

Direct page sections use 8-bit addressing:

- ``.zeropage``
- ``.directpage``
- ``.zp``

Example:

.. code-block:: c

   __attribute__((section(".zeropage")))
   int fast_var;  // Uses 2-byte DP instructions

Known Limitations
=================

Register Pressure
-----------------

The W65816 has only three general-purpose registers (A, X, Y). Complex
expressions or functions with many live variables may fail with register
allocation errors. Simplify code or split into smaller functions.

Recursion
---------

- **Direct recursion**: Supported at -O1 and higher
- **Mutual recursion** (A calls B calls A): Not detected, may produce
  incorrect results. Rewrite as iteration or use explicit stack.

32-bit Operations
-----------------

32-bit and 64-bit integer operations are not supported. Use 16-bit types
or implement wide arithmetic manually.

Runtime Library
===============

The backend requires a runtime library for multiply/divide operations:

- ``__mulhi3`` - 16-bit signed multiply
- ``__divhi3`` - 16-bit signed divide
- ``__udivhi3`` - 16-bit unsigned divide
- ``__modhi3`` - 16-bit signed modulo
- ``__umodhi3`` - 16-bit unsigned modulo

Source: ``llvm/lib/Target/W65816/runtime/w65816_runtime.s``

Assemble with:

.. code-block:: bash

   ca65 --cpu 65816 -o w65816_runtime.o w65816_runtime.s

References
==========

- `WDC W65816 Datasheet <https://www.westerndesigncenter.com/wdc/documentation/w65c816s.pdf>`_
- `WDC Programming Manual <https://www.westerndesigncenter.com/wdc/documentation/Programmanual.pdf>`_
- `Super Nintendo Development Wiki <https://snes.nesdev.org/>`_
