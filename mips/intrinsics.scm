;;;; low-level operations (MIPS)


(define-syntax-rule (%eof) ($inline "addi $v0, $s4, eof - base"))
(define-syntax-rule (%undefined) ($inline "addi $v0, $s4, undefined - base")) 

(define-syntax-rule (%slot-ref x i)
  ($inline "sll $v1, $v1, 1; addu $v1, $v1, $v0; lw $v0, 2($v1)" x i))

(define-syntax-rule (%slot-set! x i y)
  ($inline "sll $v1, $v1, 1; addu $v1, $v1, $v0; WRITE_BARRIER 2($v0), $a0; move $v0, $a0" x i y))

(define-syntax-rule (%byte-ref x i)
  ($inline "sra $v1, $v1, 1; addu $v0, $v0, $v1; lbu $v0, 4($v0); sll $v0, $v0, 1; ori $v0, $v0, 1" x i))

(define-syntax-rule (%byte-set! x i y)
  ($inline "sra $v1, $v1, 1; addu $v0, $v0, $v1; sra $a1, $a0, 1; sb $a1, 4($v0); move $v0, $a0" x i y))

;;XXX
...

(define-syntax-rule (%type-of x) ($inline "TYPE_OF" x))

(define-syntax-rule (%fixnum? x) ($inline "test rax, 1; SET_T rax; cmovz rax, FALSE" x))

(define-syntax-rule (%eq? x y) ($inline "cmp rax, r11; SET_T rax; cmovne rax, FALSE" x y))

(define-syntax-rule (%fx+ x y)
  ($inline "dec rax; add rax, r11" x y))

(define-syntax-rule (%fx- x y)
  ($inline "sub rax, r11; inc rax" x y))

(define-syntax-rule (%fx>? x y)
  ($inline "cmp rax, r11; SET_T rax; cmovle rax, FALSE" x y))

(define-syntax-rule (%fx<? x y)
  ($inline "cmp rax, r11; SET_T rax; cmovge rax, FALSE" x y))

(define-syntax-rule (%fx>=? x y)
  ($inline "cmp rax, r11; SET_T rax; cmovl rax, FALSE" x y))

(define-syntax-rule (%fx<=? x y)
  ($inline "cmp rax, r11; SET_T rax; cmovg rax, FALSE" x y))

(define-syntax-rule (%size x)
  ($inline "mov rax, [rax]; mov r11, SIZE_MASK; and rax, r11; INT2FIX rax" x))

(define-syntax-rule (%ieee754-sign x)
  ($inline "mov rax, [rax + CELLS(1)]; sar rax, 63; or rax, 1" x))

(define-syntax-rule (%ieee754-exponent x)
  ($inline "mov rax, [rax + CELLS(1)]; sar rax, 51; and rax, 0xfff; or rax, 1" x))

(define-syntax-rule (%ieee754-mantissa x)
  ($inline "mov rax, [rax + CELLS(1)]; mov r11, 0x000fffffffffffff; and rax, r11; INT2FIX rax" x))

(define-syntax-rule (%ieee754-mask x mask)
  (let ((f ($allocate #x10 1)))
    ($inline "mov r11, [r11 + CELLS(1)]; mov [rax + CELLS(1)], r11" f x) ; copy flonum
    ($inline "sar r11, 1; and [rax + CELLS(1)], r11" f mask)))

(define-syntax-rule (%ieee754-exponent-and-mantissa x)
  ($inline "mov rax, [rax + CELLS(1)]; INT2FIX rax" x))

(define-syntax-rule (%ieee754-truncate x)
  ($inline "fld qword [rax + CELLS(1)]; fisttp qword [rsp - CELLS(1)]; mov rax, [rsp - CELLS(1)]; INT2FIX rax" x))

(define-syntax-rule (%fixnum->ieee754 x)
  (let ((tmp ($allocate #x10 1)))
    ($inline "FIX2INT r11; mov [rsp - CELLS(1)], r11; fild qword [rsp - CELLS(1)]; fstp qword [rax + CELLS(1)]" tmp x)))

(define-syntax-rule ($ieee754-sin x)
  (let ((r ($allocate #x10 1)))
    (if (%fixnum? x)
	($inline "FIX2INT rax; mov [buffer], rax; fild qword [buffer]; fsin; fstp qword [r11 + CELLS(1)]" x r)
	($inline "fld qword [rax + CELLS(1)]; fsin; fstp qword [r11 + CELLS(1)]" x r))))

(define-syntax-rule (%ieee754-cos x) 
  (let ((r ($allocate #x10 1)))
    (if (exact? x)
	($inline "FIX2INT rax; mov [buffer], rax; fild qword [buffer]; fcos; fstp qword [r11 + CELLS(1)]" x r)
	($inline "fld qword [rax + CELLS(1)]; fcos; fstp qword [r11 + CELLS(1)]" x r))
    r))

(define-syntax-rule (%ieee754-tan x)
  (let ((r ($allocate #x10 1)))
    (if (%fixnum? x)
	($inline "FIX2INT rax; mov [buffer], rax; fild qword [buffer]; fptan; fstp st0; fstp qword [r11 + CELLS(1)]" x r)
	($inline "fld qword [rax + CELLS(1)]; fptan; fstp st0; fstp qword [r11 + CELLS(1)]" x r))
    r))

(define-syntax-rule (%ieee754-sqrt x)
  (let ((r ($allocate #x10 1)))
    (if (%fixnum? x)
	($inline "FIX2INT rax; mov [buffer], rax; fild qword [buffer]; fsqrt; fstp qword [r11 + CELLS(1)]" x r)
	($inline "fld qword [rax + CELLS(1)]; fsqrt; fstp qword [r11 + CELLS(1)]" x r))))

(define-syntax-rule (%ieee754-pi) 
  (let ((n ($allocate #x10 1)))
    ($inline "fldpi; fstp qword [rax + CELLS(1)]" n)))

(define-syntax-rule (%ieee754-atan1 x)
  (let ((r ($allocate #x10 1)))
    ($inline "fld qword [r11 + CELLS(1)]; fld1; fpatan; fstp qword [rax + CELLS(1)]" r x)))

(define-syntax-rule (%cells n) ($inline "shl rax, CELL_SHIFT; or rax, 1" n))

(define-syntax-rule (%bitwise-ior x y) ($inline "or rax, r11" x y))
(define-syntax-rule (%bitwise-and x y) ($inline "and rax, r11; or rax, 1" x y))
(define-syntax-rule (%bitwise-xor x y) ($inline "xor rax, r11; or rax, 1" x y))
(define-syntax-rule (%bitwise-not x) ($inline "not rax; or rax, 1" x))
(define-syntax-rule (%arithmetic-shift x y) ($inline "ARITHMETIC_SHIFT" x y))

(define-syntax-rule (%symbol-literal i)
  ($inline "FIX2INT rax; mov rax, [symbol_literals + rax * CELLS(1)]" i))

(define-syntax-rule (%terminate code)
  ($inline "mov [exit_code], rax; jmp terminate" code))

(define-syntax-rule (%argc) ($inline "mov rax, [argc]; INT2FIX rax"))

(define-syntax-rule (%argv-ref i)
  ($inline "FIX2INT rax; mov r11, [argv]; mov rax, [r11 + rax * CELLS(1)]; call alloc_zstring" i))
