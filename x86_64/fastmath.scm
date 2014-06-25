;;;; type-specific numeric operations


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
(define-syntax fxior bitwise-ior)	;XXX fxor?
(define-syntax fxxor bitwise-xor)
(define-inline (fxshr x n) (arithmetic-shift x (%fx- 0 n))) ;XXX fxarithmetic-shift-right?
(define-inline (fxshl x n) (arithmetic-shift x n)) ;XXX fxarithmetic-shift-left?
(define-inline (fxpositive? x) (%fx>? x 0))
(define-inline (fxnegative? x) (%fx<? x 0))

(define-inline (fxabs x) (if (%fx<? x 0) (fx- x) x))

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
  ($inline "movsd xmm0, [rax + CELLS(1)]; movsd xmm1, [r11 + CELLS(1)]; ucomisd xmm0, xmm1; lea rax, [FALSE + CELLS(2)]; cmovne rax, FALSE" x y))

(define-inline (fl> x y)
  ($inline "movsd xmm0, [rax + CELLS(1)]; movsd xmm1, [r11 + CELLS(1)]; ucomisd xmm0, xmm1; lea rax, [FALSE + CELLS(2)]; cmovle rax, FALSE" x y))

(define-inline (fl< x y)
  ($inline "movsd xmm0, [rax + CELLS(1)]; movsd xmm1, [r11 + CELLS(1)]; ucomisd xmm0, xmm1; lea rax, [FALSE + CELLS(2)]; cmovge rax, FALSE" x y))

(define-inline (fl>= x y)
  ($inline "movsd xmm0, [rax + CELLS(1)]; movsd xmm1, [r11 + CELLS(1)]; ucomisd xmm0, xmm1; lea rax, [FALSE + CELLS(2)]; cmovl rax, FALSE" x y))

(define-inline (fl<= x y)
  ($inline "movsd xmm0, [rax + CELLS(1)]; movsd xmm1, [r11 + CELLS(1)]; ucomisd xmm0, xmm1; lea rax, [FALSE + CELLS(2)]; cmovg rax, FALSE" x y))

(define-inline (flround x) (%ieee754-round x))
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
