;;;; low-level operations (x86_64)


(define-syntax define-syntax-rule
  (syntax-rules ___ ()
    ((_ (name args ___) rule)
     (define-syntax name
       (syntax-rules ()
	 ((_ args ___) rule))))))


(define-syntax %cell-shift ($inline "mov rax, FIX(CELL_SHIFT)"))

(define-syntax-rule (%eof) ($inline "mov rax, eof"))
(define-syntax-rule (%undefined) ($inline "mov rax, undefined")) 

(define-syntax-rule (%slot-ref x i)
  ($inline "shl r11, 2; mov rax, [rax + r11 + 4]" x i))

(define-syntax-rule (%slot-set! x i y)
  ($inline "shl r11, 2; WRITE_BARRIER [rax + r11 + 4], r15; mov rax, r15" x i y))

(define-syntax-rule (%byte-ref x i)
  ($inline "FIX2INT r11; add rax, r11; mov al, [rax + CELLS(1)]; and rax, 0xff; INT2FIX rax" x i))

(define-syntax-rule (%byte-set! x i y)
  ($inline "FIX2INT r11; add rax, r11; xchg rax, r15; FIX2INT rax; mov [r15 + CELLS(1)], al; mov rax, r15" x i y))

(define-syntax-rule (%type-of x)
  ($inline "test rax, 1; if z; mov rax, [rax]; shr rax, HEADER_SHIFT; and rax, 0x7f; INT2FIX rax; else; mov rax, (TYPENUMBER(FIXNUM) << 1) | 1; endif" x))

(define-syntax-rule (%bits-of x)
  ($inline "mov rax, [rax]; shr rax, HEADER_SHIFT - 1; or rax, 1" x))

(define-syntax-rule (%fixnum? x) ($inline "test rax, 1; SET_T rax; cmovz rax, FALSE" x))

(define-syntax-rule (%eq? x y) ($inline "cmp rax, r11; SET_T rax; cmovne rax, FALSE" x y))

(define-syntax-rule (%fx+ x y)
  ($inline "dec rax; add rax, r11" x y))

(define-syntax-rule (%fx- x y)
  ($inline "sub rax, r11; inc rax" x y))

(define-syntax-rule (%fx* x y)
  ($inline "FIX2INT rax; FIX2INT r11; push rdx; imul r11; pop rdx; INT2FIX rax" x y))

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
	($inline "fld qword [rax + CELLS(1)]; fsin; fstp qword [r11 + CELLS(1)]" x r))
    r))

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
	($inline "fld qword [rax + CELLS(1)]; fsqrt; fstp qword [r11 + CELLS(1)]" x r))
    r))

(define-syntax-rule (%ieee754-asin x)
  (let ((r ($allocate #x10 1)))
    (if (%fixnum? x)
	($inline "FIX2INT rax; mov [buffer], rax; fild qword [buffer]; fld st0; fmul st0, st0; fld1; fsubr; fsqrt; fpatan; fstp qword [r11 + CELLS(1)]" x r)
	($inline "fld qword [rax + CELLS(1)]; fld st0; fmul st0, st0; fld1; fsubr; fsqrt; fpatan; fstp qword [r11 + CELLS(1)]" x r))
    r))

(define-syntax-rule (%ieee754-acos x)
  (let ((r ($allocate #x10 1)))
    (if (%fixnum? x)
	($inline "FIX2INT rax; mov [buffer], rax; fild qword [buffer]; fld st0; fmul st0, st0; fld1; fsubr; fsqrt; fxch st1; fpatan; fstp qword [r11 + CELLS(1)]" x r)
	($inline "fld qword [rax + CELLS(1)]; fld st0; fmul st0, st0; fld1; fsubr; fsqrt; fxch st1; fpatan; fstp qword [r11 + CELLS(1)]" x r))
    r))

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

(define-syntax-rule (%arithmetic-shift x y)
  ($inline "push rcx; mov rcx, r11; FIX2INT rax; FIX2INT rcx; if l; neg rcx; sar rax, cl; else; shl rax, cl; endif; INT2FIX rax; pop rcx"
	   x y))

(define-syntax-rule (%symbol-literal i)
  ($inline "FIX2INT rax; lea r11, [rel symbol_literals]; mov rax, [r11 + rax * CELLS(1)]" 
	   i))

(define-syntax-rule (%terminate code)
  ($inline "mov [exit_code], rax; jmp terminate" code))

(define-syntax-rule (%argc) ($inline "mov rax, [argc]; INT2FIX rax"))

(define-syntax-rule (%argv-ref i)
  ($inline "FIX2INT rax; mov r11, [argv]; mov rax, [r11 + rax * CELLS(1)]; call alloc_zstring" i))

(define-syntax-rule (%fx-divmod x y k)
  ;; unsigned divide!
  (let ((q ($inline "FIX2INT rax; FIX2INT r11; push rdx; xor rdx, rdx; div r11; mov r15, rdx; pop rdx; INT2FIX rax" x y)))
    ;; bold hack: we assume r15 will not be clobbered by "k"
    (k q ($inline "mov rax, r15; INT2FIX rax"))))

(define-syntax-rule (%free)
  ($inline "mov rax, [fromspace_end]; sub rax, ALLOC; INT2FIX rax"))
