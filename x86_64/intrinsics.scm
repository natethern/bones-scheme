;;;; low-level operations (x86_64)


(define-syntax-rule (%slot-ref x i) ($inline "SLOT_REF" x i))
(define-syntax-rule (%slot-set! x i y) ($inline "SLOT_SET" x i y))
(define-syntax-rule (%byte-ref x i) ($inline "BYTE_REF" x i))
(define-syntax-rule (%byte-set! x i y) ($inline "BYTE_SET" x i y))

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

(define-syntax-rule (%cells n) ($inline "shl rax, CELL_SHIFT; or rax, 1" n))
