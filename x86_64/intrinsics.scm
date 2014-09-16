;;;; low-level intrinsic operations (x86_64)


;; shift-value for computing the number of bytes per "cell" (word)
(define-syntax %cell-shift ($inline "mov rax, FIX(CELL_SHIFT)"))

;; some unique values
(define-syntax-rule (%eof) ($inline "mov rax, eof"))
(define-syntax-rule (%undefined) ($inline "mov rax, undefined")) 

;; slot accessors
(define-syntax-rule (%slot-ref x i)
  ($inline "CHECK_SLOT_ACCESS rax, r11; shl r11, 2; mov rax, [rax + r11 + 4]" x i))

(define-syntax-rule (%slot-set! x i y)
  ($inline "CHECK_SLOT_ACCESS rax, r11; shl r11, 2; WRITE_BARRIER [rax + r11 + 4], r15; mov rax, r15" x i y))

;; byte-accessors
(define-syntax-rule (%byte-ref x i)
  ($inline
   "CHECK_BYTE_ACCESS rax, r11; FIX2INT r11; add rax, r11; mov al, [rax + CELLS(1)]; and rax, 0xff; INT2FIX rax" 
   x i))

(define-syntax-rule (%byte-set! x i y)
  ($inline
   "CHECK_BYTE_ACCESS rax, r11; FIX2INT r11; add rax, r11; xchg rax, r15; FIX2INT rax; mov [r15 + CELLS(1)], al; mov rax, FALSE" 
   x i y))

;; extract block-type, with a special case for immediate fixnums
(define-syntax-rule (%type-of x)
  ($inline
   "test rax, 1; if z; mov rax, [rax]; shr rax, HEADER_SHIFT; and rax, 0x7f; INT2FIX rax; else; mov rax, (TYPENUMBER(FIXNUM) << 1) | 1; endif" 
   x))

;; extract block-type, requires a block
(define-syntax-rule (%bits-of x)
  ($inline "mov rax, [rax]; shr rax, HEADER_SHIFT - 1; or rax, 1" x))

;; predicate for determining whether a value is a fixnum
(define-syntax-rule (%fixnum? x)
  ($inline-test "test rax, 1" "ne" x))

;; compare identity of two values
(define-syntax-rule (%eq? x y)
  ($inline-test "cmp rax, r11" "eq" x y))

;; fixnum arithmetic
(define-syntax-rule (%fx+ x y)
  ($inline "dec rax; add rax, r11" x y))

(define-syntax-rule (%fx- x y)
  ($inline "sub rax, r11; inc rax" x y))

(define-syntax-rule (%fx* x y)
  ($inline "FIX2INT rax; FIX2INT r11; push rdx; imul r11; pop rdx; INT2FIX rax" x y))

(define-syntax-rule (%fx/ x y)
  ($inline "FIX2INT rax; push rdx; cqo; FIX2INT r11; idiv r11; pop rdx; INT2FIX rax" x y))

(define-syntax-rule (%fx-divmod x y k)
  ;; unsigned divide!
  (let ((q ($inline "FIX2INT rax; FIX2INT r11; push rdx; xor rdx, rdx; div r11; mov r15, rdx; pop rdx; INT2FIX rax" x y)))
    ;; bold hack: we assume r15 will not be clobbered by "k"
    (k q ($inline "mov rax, r15; INT2FIX rax"))))

;; fixnum comparisons
(define-syntax-rule (%fx>? x y)
  ($inline-test "cmp rax, r11" "gt" x y))

(define-syntax-rule (%fx<? x y)
  ($inline-test "cmp rax, r11" "lt" x y))

(define-syntax-rule (%fx>=? x y)
  ($inline-test "cmp rax, r11" "ge" x y))

(define-syntax-rule (%fx<=? x y)
  ($inline-test "cmp rax, r11" "le" x y))

;; extract block-size (bytes or cells)
(define-syntax-rule (%size x)
  ($inline "mov rax, [rax]; mov r11, SIZE_MASK; and rax, r11; INT2FIX rax" x))

;; operations on IEEE-754 doubles
(define-syntax-rule (%ieee754-sign x)
  ($inline "mov rax, [rax + CELLS(1)]; sar rax, 63; or rax, 1" x))

(define-syntax-rule (%ieee754-exponent x)
  ($inline "mov rax, [rax + CELLS(1)]; sar rax, 51; and rax, 0xfff; or rax, 1" x))

(define-syntax-rule (%ieee754-mantissa x)
  ($inline "mov rax, [rax + CELLS(1)]; mov r11, 0x000fffffffffffff; and rax, r11; INT2FIX rax" x))

(define-syntax-rule (%ieee754-mask x mask)
  (let ((f ($allocate #x10 8)))
    ($inline "mov r11, [r11 + CELLS(1)]; mov [rax + CELLS(1)], r11" f x) ; copy flonum
    ($inline "sar r11, 1; and [rax + CELLS(1)], r11" f mask)))

(define-syntax-rule (%ieee754-exponent-and-mantissa x)
  ($inline "mov rax, [rax + CELLS(1)]; INT2FIX rax" x))

(define-syntax-rule (%ieee754-truncate x)
  ($inline "fld qword [rax + CELLS(1)]; fisttp qword [rsp - CELLS(1)]; mov rax, [rsp - CELLS(1)]; INT2FIX rax" x))

(define-syntax-rule (%ieee754-round x)
  (let ((tmp ($allocate #x10 8)))
    ($inline "fld qword [r11 + CELLS(1)]; frndint; fstp qword [rax + CELLS(1)]" tmp x)))

(define-syntax-rule (%fixnum->ieee754 x)
  (let ((tmp ($allocate #x10 8)))
    ($inline "FIX2INT r11; mov [rsp - CELLS(1)], r11; fild qword [rsp - CELLS(1)]; fstp qword [rax + CELLS(1)]" tmp x)))

;; trigonometric IEEE-754 operations
(define-syntax-rule (%ieee754-sin x)
  (let ((r ($allocate #x10 8)))
    (if (%fixnum? x)
	($inline "FIX2INT rax; mov [buffer], rax; fild qword [buffer]; fsin; fstp qword [r11 + CELLS(1)]" x r)
	($inline "fld qword [rax + CELLS(1)]; fsin; fstp qword [r11 + CELLS(1)]" x r))
    r))

(define-syntax-rule (%ieee754-cos x) 
  (let ((r ($allocate #x10 8)))
    (if (exact? x)
	($inline "FIX2INT rax; mov [buffer], rax; fild qword [buffer]; fcos; fstp qword [r11 + CELLS(1)]" x r)
	($inline "fld qword [rax + CELLS(1)]; fcos; fstp qword [r11 + CELLS(1)]" x r))
    r))

(define-syntax-rule (%ieee754-tan x)
  (let ((r ($allocate #x10 8)))
    (if (%fixnum? x)
	($inline "FIX2INT rax; mov [buffer], rax; fild qword [buffer]; fptan; fstp st0; fstp qword [r11 + CELLS(1)]" x r)
	($inline "fld qword [rax + CELLS(1)]; fptan; fstp st0; fstp qword [r11 + CELLS(1)]" x r))
    r))

(define-syntax-rule (%ieee754-asin x)
  (let ((r ($allocate #x10 8)))
    (if (%fixnum? x)
	($inline "FIX2INT rax; mov [buffer], rax; fild qword [buffer]; fld st0; fmul st0, st0; fld1; fsubr; fsqrt; fpatan; fstp qword [r11 + CELLS(1)]" x r)
	($inline "fld qword [rax + CELLS(1)]; fld st0; fmul st0, st0; fld1; fsubr; fsqrt; fpatan; fstp qword [r11 + CELLS(1)]" x r))
    r))

(define-syntax-rule (%ieee754-acos x)
  (let ((r ($allocate #x10 8)))
    (if (%fixnum? x)
	($inline "FIX2INT rax; mov [buffer], rax; fild qword [buffer]; fld st0; fmul st0, st0; fld1; fsubr; fsqrt; fxch st1; fpatan; fstp qword [r11 + CELLS(1)]" x r)
	($inline "fld qword [rax + CELLS(1)]; fld st0; fmul st0, st0; fld1; fsubr; fsqrt; fxch st1; fpatan; fstp qword [r11 + CELLS(1)]" x r))
    r))

(define-syntax-rule (%ieee754-pi) 
  (let ((n ($allocate #x10 8)))
    ($inline "fldpi; fstp qword [rax + CELLS(1)]" n)))

(define-syntax-rule (%ieee754-atan1 x)
  (let ((r ($allocate #x10 8)))
    ($inline "fld qword [r11 + CELLS(1)]; fld1; fpatan; fstp qword [rax + CELLS(1)]" r x)))

(define-syntax-rule (%ieee754-log x)
  (let ((r ($allocate #x10 8)))
    ($inline "fldln2; fld qword [r11 + CELLS(1)]; fyl2x; fstp qword [rax + CELLS(1)]" r (exact->inexact x))))

;; IEEE-754 square root
(define-syntax-rule (%ieee754-sqrt x)
  (let ((r ($allocate #x10 8)))
    (if (%fixnum? x)
	($inline "FIX2INT rax; mov [buffer], rax; fild qword [buffer]; fsqrt; fstp qword [r11 + CELLS(1)]" x r)
	($inline "fld qword [rax + CELLS(1)]; fsqrt; fstp qword [r11 + CELLS(1)]" x r))
    r))

(define-syntax-rule (%ieee754-nan) ($inline "lea rax, [ieee754_nan]"))
(define-syntax-rule (%ieee754-infinity) ($inline "lea rax, [ieee754_inf]"))
(define-syntax-rule (%ieee754-negative-infinity) ($inline "lea rax, [ieee754_ninf]"))

;; compute size for a given number of cells
(define-syntax-rule (%cells n) ($inline "shl rax, CELL_SHIFT; or rax, 1" n))

;; bitwise operations, only valid on fixnums
(define-syntax-rule (%bitwise-ior x y) ($inline "or rax, r11" x y))
(define-syntax-rule (%bitwise-and x y) ($inline "and rax, r11; or rax, 1" x y))
(define-syntax-rule (%bitwise-xor x y) ($inline "xor rax, r11; or rax, 1" x y))
(define-syntax-rule (%bitwise-not x) ($inline "not rax; or rax, 1" x))

(define-syntax-rule (%arithmetic-shift x y)
  ($inline "push rcx; mov rcx, r11; FIX2INT rax; FIX2INT rcx; if l; neg rcx; sar rax, cl; else; shl rax, cl; endif; INT2FIX rax; pop rcx"
	   x y))

;; fetch symbol-literal from internal table for adding it to the symbol-table
(define-syntax-rule (%symbol-literal i)
  (cond-expand
    (pic
     ($inline
      "FIX2INT rax; lea r11, [symbol_literals]; mov rax, [r11 + rax * CELLS(1)]" 
      i))
    (else
     ($inline "FIX2INT rax; mov rax, [symbol_literals + rax * CELLS(1)]" i))))

;; terminate program
(define-syntax-rule (%terminate code)
  ($inline "mov [exit_code], rax; jmp terminate" code))

;; get command-line argument count
(define-syntax-rule (%argc) ($inline "mov rax, [argc]; INT2FIX rax"))

;; get command-line argument by index
(define-syntax-rule (%argv-ref i)
  ($inline "FIX2INT rax; mov r11, [argv]; mov rax, [r11 + rax * CELLS(1)]; call alloc_zstring" i))

;; get number of remaining space in active half of heap
(define-syntax-rule (%free)
  ($inline "mov rax, [fromspace_end]; sub rax, ALLOC; INT2FIX rax"))

;; check whether signals are pending
(define-syntax-rule (%check-interrupts)
  ($inline "mov rax, [pending_signals]; INT2FIX rax"))

;; clear a pending interrupt
(define-syntax-rule (%clear-interrupt num)
  ($inline "mov r11, 1; push rcx; mov rcx, rax; FIX2INT rcx; dec rcx; shl r11, cl; pop rcx; not r11; and [pending_signals], r11" num))
