;;;; type-specific numeric operations


(define-syntax fixnum? exact?)

(define-inline (fx+ x y) (%fx+ x y))

(define-syntax fx-
  (case-lambda 
    ((x y) (%fx- x y))
    ((x) (%fx- 0 x))))

(define-inline (fx* x y) (%fx* x y))
(define-inline (fx/ x y) (%fx/ x y))
(define-syntax fx= eq?)
(define-inline (fx> x y) (%fx>? x y))
(define-inline (fx< x y) (%fx<? x y))
(define-inline (fx>= x y) (%fx>=? x y))
(define-inline (fx<= x y) (%fx<=? x y))
(define-syntax fxand bitwise-and)
(define-syntax fxior bitwise-ior)      
(define-syntax fxxor bitwise-xor)
(define-syntax fxnot bitwise-not)
(define-inline (fxarithmetic-shift-right x n) (arithmetic-shift x (%fx- 0 n)))
(define-inline (fxarithmetic-shift-left x n) (arithmetic-shift x n))
(define-inline (fxpositive? x) (%fx>? x 0))
(define-inline (fxnegative? x) (%fx<? x 0))
(define-inline (fxeven? x) (%eq? 0 (bitwise-and x 1)))
(define-inline (fxodd? x) (not (%eq? 0 (bitwise-and x 1))))
(define-inline (fxzero? x) (%eq? 0 x))

(define-inline (fxabs x) (if (%fx<? x 0) (fx- x) x))

(define-inline (fxremainder x y)
  ($inline "FIX2INT rax; FIX2INT r11; push rdx; cqo; idiv r11; mov rax, rdx; pop rdx; INT2FIX rax" x y))

(define-inline (fxmodulo x y)
  (let ((z (fxremainder x y)))
    (if (%fx<? y 0)
	(if (%fx>? z 0) (%fx+ z y) z)
	(if (%fx<? z 0) (%fx+ z y) z))))

(define-inline (fxmax x y) (if (%fx>? x y) x y))
(define-inline (fxmin x y) (if (%fx<? x y) x y))

(define-inline (fl+ x y)
  (let ((r ($allocate #x10 1)))
    ($inline "movsd xmm0, [rax + CELLS(1)]; movsd xmm1, [r11 + CELLS(1)]; addsd xmm0, xmm1; movsd [r15 + CELLS(1)], xmm0" x y r)
    r))

(define-syntax fl-
  (case-lambda
    ((x y)
     (let ((r ($allocate #x10 1)))
       ($inline "movsd xmm0, [rax + CELLS(1)]; movsd xmm1, [r11 + CELLS(1)]; subsd xmm0, xmm1; movsd [r15 + CELLS(1)], xmm0" x y r)
       r))
    ((x) (fl- 0.0 x))))

(define-inline (fl* x y)
  (let ((r ($allocate #x10 1)))
    ($inline "movsd xmm0, [rax + CELLS(1)]; movsd xmm1, [r11 + CELLS(1)]; mulsd xmm0, xmm1; movsd [r15 + CELLS(1)], xmm0" x y r)
    r))

(define-syntax fl/
  (case-lambda 
    ((x y)
     (let ((r ($allocate #x10 1)))
       ($inline "movsd xmm0, [rax + CELLS(1)]; movsd xmm1, [r11 + CELLS(1)]; divsd xmm0, xmm1; movsd [r15 + CELLS(1)], xmm0" x y r)
       r))
    ((x) (fl/ 1.0 x))))

(define-inline (fl= x y)
  ($inline-test "movsd xmm0, [rax + CELLS(1)]; movsd xmm1, [r11 + CELLS(1)]; ucomisd xmm0, xmm1" "eq" x y))

(define-inline (fl> x y)
  ($inline-test "movsd xmm0, [rax + CELLS(1)]; movsd xmm1, [r11 + CELLS(1)]; ucomisd xmm0, xmm1" "a" x y))

(define-inline (fl< x y)
  ($inline-test "movsd xmm0, [rax + CELLS(1)]; movsd xmm1, [r11 + CELLS(1)]; ucomisd xmm0, xmm1" "b" x y))

(define-inline (fl>= x y)
  ($inline-test "movsd xmm0, [rax + CELLS(1)]; movsd xmm1, [r11 + CELLS(1)]; ucomisd xmm0, xmm1" "ae" x y))

(define-inline (fl<= x y)
  ($inline-test "movsd xmm0, [rax + CELLS(1)]; movsd xmm1, [r11 + CELLS(1)]; ucomisd xmm0, xmm1" "be" x y))

(define-inline (flround x) (%ieee754-round x))

;; these are not worth the trouble
(define-syntax flfloor floor)
(define-syntax flceiling ceiling)
(define-syntax fltruncate truncate)
(define-syntax flfloor floor)
(define-syntax flinteger? integer?)

(define-inline (flsin x) (%ieee754-sin x))
(define-inline (flcos x) (%ieee754-cos x))
(define-inline (fltan x) (%ieee754-tan x))
(define-inline (flasin x) (%ieee754-asin x))
(define-inline (flacos x) (%ieee754-acos x))

(define %pi (%ieee754-pi))
(define %pi/2 (fl/ %pi 2.0))

(define-syntax flatan
  (case-lambda
    ((x) (%ieee754-atan1 x))
    ((y x)
     (cond ((%= x 0) (if (fl> y 0) %pi/2 (flneg %pi/2))) ; y == 0 -> undefined
	   ((%> x 0) (%ieee754-atan1 (fl/ y x)))
	   ((%< y 0) (fl- (%ieee754-atan1 (fl/ y x)) %pi))
	   (else (fl+ (%ieee754-atan1 (fl/ y x)) %pi))))))

(define-inline (flsqrt x) (%ieee754-sqrt x))
(define-inline (flexpt x y) ($inline "call flonum_expt" x y))

(let-syntax ((e 2.7182818284590452353602874))
  (define-inline (flexp x) ($inline "call flonum_expr" x e)))

(define-inline (flabs x) (if (flnegative? x) (fl- x) x))

(define-inline (flpositive? x) (fl> x 0))
(define-inline (flnegative? x) (fl< x 0))

(define-syntax flonum? inexact?)

(define-inline (flzero? x) (eqv? x 0.0))
(define-inline (fleven? x) (fxzero? (fl/ x 2)))
(define-inline (flodd? x) (not (fxzero? (fl/ x 2))))

(define-inline (flmax x y) (if (fl> x y) x y))
(define-inline (flmin x y) (if (fl< x y) x y))

(define-inline (flfinite? x)
  (or (not (%eq? 2047 (%ieee754-exponent x)))   ; inf or nan
      (not (%eq? 0 (%ieee754-mantissa x)))))    ; nan

(define-inline (flnan? x)
  (and (%eq? 2047 (%ieee754-exponent x))
       (not (%eq? 0 (%ieee754-mantissa x)))))

(define-inline (fllog x) (%ieee754-log x))
