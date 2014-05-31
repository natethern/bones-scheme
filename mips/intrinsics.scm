;;;; low-level operations (MIPSel)

^
(define-syntax-rule (%eof) ($inline "addiu $v0, BASE, eof - base"))
(define-syntax-rule (%undefined) ($inline "addiu $v0, BASE, undefined - base")) 

(define-syntax-rule (%slot-ref x i)
  ($inline "sll $v1, $v1, 1; addu $v1, $v1, $v0; lw $v0, 2($v1)" x i))

(define-syntax-rule (%slot-set! x i y)
  ($inline "sll $v1, $v1, 1; addu $v1, $v1, $v0; addiu $v1, $v1, 2; WRITE_BARRIER $v1, $a0; move $v0, $a0" x i y))

(define-syntax-rule (%byte-ref x i)
  ($inline "sra $v1, $v1, 1; addu $v0, $v0, $v1; lbu $v0, 4($v0); sll $v0, $v0, 1; ori $v0, $v0, 1" x i))

(define-syntax-rule (%byte-set! x i y)
  ($inline "sra $v1, $v1, 1; addu $v0, $v0, $v1; sra $a1, $a0, 1; sb $a1, 4($v0); move $v0, $a0" x i y))

(define-syntax-rule (%type-of x)
  ($inline "andi $v0, $v0, 1; bne $v0, $zero, 1f; lbu $v0, 3($v0); INT2FIX $v0, $v0; j 2f; 1: li $v0, (TYPENUMBER(FIXNUM) << 1) | 2; 2:"
	   x))

(define-syntax-rule (%fixnum? x) ($inline "andi $v1, $v0, 1; sll $v1, $v1, 3; addu $v0, FALSE, $v1" x))

(define-syntax-rule (%eq? x y) ($inline "seq $v0, $v0, $v1; sll $v0, $v0, 3; addu $v0, FALSE, $v0" x y))

(define-syntax-rule (%fx+ x y)
  ($inline "addiu $v0, $v0, 1; addu $v0, $v0, $v1" x y))

(define-syntax-rule (%fx- x y)
  ($inline "sub $v0, $v0, $v1; addiu $v0, $v0, 1" x y))

(define-syntax-rule (%fx* x y)
  ($inline "FIX2INT $v0, $v0; FIX2INT $v1, $v1; mul $v0, $v0, $v1; INT2FIX $v0, $v0" x y))

(define-syntax-rule (%fx>? x y)
  ($inline "sgt $v0, $v0, $v1; sll $v0, $v0, 3; addu $v0, FALSE, $v0" x y))

(define-syntax-rule (%fx<? x y)
  ($inline "slt $v0, $v0, $v1; sll $v0, $v0, 3; addu $v0, FALSE, $v0" x y))

(define-syntax-rule (%fx>=? x y)
  ($inline "sge $v0, $v0, $v1; sll $v0, $v0, 3; addu $v0, FALSE, $v0" x y))

(define-syntax-rule (%fx<=? x y)
  ($inline "sle $v0, $v0, $v1; sll $v0, $v0, 3; addu $v0, FALSE, $v0" x y))

(define-syntax-rule (%size x)
  ($inline "lw $v0, 0($v0); li $v1, SIZE_MASK; and $v0, $v0, $v1; INT2FIX $v0, $v0" x))

;;;XXX no ieee754 intrinsics, yet

(define-syntax-rule (%cells n) ($inline "sll $v0, CELL_SHIFT; ori $v0, $v0, 1" n))

(define-syntax-rule (%bitwise-ior x y) ($inline "or $v0, $v0, $v1" x y))
(define-syntax-rule (%bitwise-and x y) ($inline "and $v0, $v0, $v1; ori $v0, $v0, 1" x y))
(define-syntax-rule (%bitwise-xor x y) ($inline "xor $v0, $v0, $v1; ori $v0, $v0, 1" x y))
(define-syntax-rule (%bitwise-not x) ($inline "not $v0, $v0; ori $v0, $v0, 1" x))

(define-syntax-rule (%arithmetic-shift x y)
  ($inline "sra $v0, $v0, 1; sra $v1, $v1, 1; bge $v1, $zero, 1f; neg $v1, $v1; srav $v0, $v0, $v1; j 2f; 1: sllv $v0, $v0, $v1; 2: INT2FIX $v0, $v0" x y))

(define-syntax-rule (%symbol-literal i)
  ($inline "sra $v0, $v0, 1; sll $v0, $v0, 2; addu $v0, $v0, BASE; lw $v0, (symbol_literals - base)($v0)" i))

(define-syntax-rule (%terminate code)
  ($inline "sw $v0, (exit_code - base)(BASE); j terminate" code))

(define-syntax-rule (%argc) ($inline "lw $v0, (argc - base)(BASE); INT2FIX $v0, $v0"))

(define-syntax-rule (%argv-ref i)
  ($inline "sll $v0, $v0, 1; lw $v1, (argv - base)(BASE); addu $v0, $v0, $v1; lw $v0, -2($v0); jal alloc_zstring" i))

(define-syntax-rule (%fx-divmod x y k)
  ;; note: sign of result with negative arguments is undefined
  (begin
    ($inline "FIX2INT $v0, $v0; FIX2INT $v1, $v1; div $v0, $v1" x y)
    (k ($inline "mflo $v0") ($inline "mfhi $v0"))))
