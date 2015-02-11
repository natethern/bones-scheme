;;;; R5RS library for files compiled with barebones


(define-syntax-rule (when x y z ...)
  (if x (begin y z ...)))

(define-syntax-rule (unless x y z ...)
  (if (not x) (begin y z ...)))

(define-syntax optional
  (syntax-rules ()
    ((_ x y) (if (pair? x) (car x) y))
    ((_ x) (optional x #f))))

(define-syntax-rule (define-inline (name . llist) body ...)
  (define-syntax name
    (lambda llist body ...)))

(define-syntax-rule (case-lambda (llist . body) ...)
  ($case-lambda (lambda llist . body) ...))


(define (%list . lst) lst)

(define-syntax list %list)

(define-inline (eof-object) (%eof))

(define (void . args) (%undefined))

(define-inline (car x) (%slot-ref x 0))
(define-inline (cdr x) (%slot-ref x 1))
(define-inline (caar x) (car (car x)))
(define-inline (cadr x) (car (cdr x)))
(define-inline (cdar x) (cdr (car x)))
(define-inline (cddr x) (cdr (cdr x)))
(define-inline (caaar x) (car (car (car x))))
(define-inline (caadr x) (car (car (cdr x))))
(define-inline (cadar x) (car (cdr (car x))))
(define-inline (caddr x) (car (cdr (cdr x))))
(define-inline (cdaar x) (cdr (car (car x))))
(define-inline (cdadr x) (cdr (car (cdr x))))
(define-inline (cddar x) (cdr (cdr (car x))))
(define-inline (cdddr x) (cdr (cdr (cdr x))))
(define-inline (caaaar x) (car (car (car (car x)))))
(define-inline (caaadr x) (car (car (car (cdr x)))))
(define-inline (caadar x) (car (car (cdr (car x)))))
(define-inline (caaddr x) (car (car (cdr (cdr x)))))
(define-inline (cadaar x) (car (cdr (car (car x)))))
(define-inline (cadadr x) (car (cdr (car (cdr x)))))
(define-inline (caddar x) (car (cdr (cdr (car x)))))
(define-inline (cadddr x) (car (cdr (cdr (cdr x)))))
(define-inline (cdaaar x) (cdr (car (car (car x)))))
(define-inline (cdaadr x) (cdr (car (car (cdr x)))))
(define-inline (cdadar x) (cdr (car (cdr (car x)))))
(define-inline (cdaddr x) (cdr (car (cdr (cdr x)))))
(define-inline (cddaar x) (cdr (cdr (car (car x)))))
(define-inline (cddadr x) (cdr (cdr (car (cdr x)))))
(define-inline (cdddar x) (cdr (cdr (cdr (car x)))))
(define-inline (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define-inline (set-car! x y) (%slot-set! x 0 y))
(define-inline (set-cdr! x y) (%slot-set! x 1 y))

(define-inline (not x) (if x #f #t))

(define-inline (eq? x y) (%eq? x y))
(define-inline (eqv? x y) ($inline "CALL structurally_equal" x y))
(define-inline (equal? x y) ($inline "CALL recursively_equal" x y))

(define-inline (pair? x) (%eq? (%type-of x) 2))
(define-inline (null? x) (%eq? x '()))
(define-inline (symbol? x) (%eq? (%type-of x) 1))
(define-inline (string? x) (%eq? (%type-of x) #x11))
(define-inline (bytevector? x) (%eq? (%type-of x) #x12))
(define-inline (procedure? x) (%eq? (%type-of x) #x20))
(define-inline (vector? x) (%eq? (%type-of x) 3))
(define-inline (char? x) (%eq? (%type-of x) 4))
(define-inline (eof-object? x) (%eq? x (eof-object)))
(define-inline (boolean? x) (%eq? (%type-of x) 7))
(define-inline (port? x) (%eq? (%type-of x) 8))
(define-inline (record? x) (%eq? (%type-of x) 10))
(define-inline (input-port? x) (and (port? x) (%slot-ref x 1)))
(define-inline (output-port? x) (and (port? x) (not (%slot-ref x 1))))
(define-inline (promise? x) (%eq? (%type-of x) 9))

(define-inline (number? x) (or (%fixnum? x) (%eq? (%type-of x) #x10)))
(define-syntax real? number?)
(define-inline (exact? x) (%fixnum? x))

(define-inline (rational? x)
  (or (exact? x)
      (not (%eq? 2047 (%ieee754-exponent x))))) ; inf or nan

(define-inline (inexact? x) 
  (and (not (%fixnum? x))
       (%eq? (%type-of x) #x10)))

(define-inline (char->integer x) (%slot-ref x 0))

(define-inline (integer->char x)
  (or ($inline "CALL lookup_char" x) ($allocate 4 1 x)))

(define %= ($primitive "numerically_equal"))
(define %> ($primitive "numerically_greater"))
(define %< ($primitive "numerically_less"))
(define >= ($primitive "numerically_greater_or_equal"))
(define <= ($primitive "numerically_less_or_equal"))

(define %* ($primitive "multiply_numbers"))
(define %+ ($primitive "add_numbers"))
(define %- ($primitive "subtract_numbers"))
(define %/ ($primitive "divide_numbers"))
(define max ($primitive "maximize_numbers"))
(define min ($primitive "minimize_numbers"))

(define-syntax + %+)
(define-syntax - %-)
(define-syntax * %*)
(define-syntax / %/)

(define-syntax = %=)
(define-syntax > %>)
(define-syntax < %<)

(define-inline (bitwise-ior x y) (%bitwise-ior x y))
(define-inline (bitwise-and x y) (%bitwise-and x y))
(define-inline (bitwise-xor x y) (%bitwise-xor x y))
(define-inline (bitwise-not x) (%bitwise-not x))
(define-inline (arithmetic-shift x y) (%arithmetic-shift x y))

(define %allocate-block ($primitive "alloc_block"))

(define-inline (negative? x)
  (%fx<? (if (exact? x) x (%ieee754-sign x)) 0))

(define-inline (positive? x)
  (cond ((exact? x) (%fx>? x 0))
	((%eq? 0 (%ieee754-sign x)) (not (%eq? 0 (%ieee754-exponent-and-mantissa x))))
	(else #f)))

(define-inline (zero? n)
  (%eq? (if (exact? n) n (%ieee754-exponent-and-mantissa n)) 0))

(define-inline (even? n)
  (zero? (if (exact? n) (bitwise-and n 1) (%/ n 2))))

(define-inline (odd? n) (not (even? n)))

(define-inline (finite? x)
  (or (exact? x)
      (not (%eq? 2047 (%ieee754-exponent x)))   ; inf or nan
      (not (%eq? 0 (%ieee754-mantissa x)))))    ; nan

(define-inline (nan? x)
  (and (not (exact? x))
       (%eq? 2047 (%ieee754-exponent x))
       (not (%eq? 0 (%ieee754-mantissa x)))))

(define-inline (inexact->exact x)
  (if (exact? x)
      x
      (%ieee754-truncate x)))

(define-inline (exact->inexact x)
  (if (exact? x)
      (%fixnum->ieee754 x)
      x))

(define-inline (integer? x)
  (or (exact? x)
      (let ((e (%ieee754-exponent x))
	    (m (%ieee754-mantissa x)))
	(cond ((%eq? #x7ff e) #f)	; inf or nan
	      ((%eq? 0 e) (%eq? 0 m))	; zero or denormal
	      ((%fx>=? e 1075))	; exceeds precision of mantissa, so must be integer (1023 + 52)
	      ((%fx<? e 1023) #f) ; no integer part, so must be fraction
	      (else (%eq? 0 (arithmetic-shift m (%fx- e 1012)))))))) ; e - 1023 + 11 (1 bit gets lost in fixnum)

(define-inline (quotient x y) ($inline "CALL quotient" x y))
(define-inline (remainder x y) ($inline "CALL remainder" x y))

(define (modulo x y)
  (let ((z (remainder x y)))
    (if (negative? y)
	(if (positive? z) (%+ z y) z)
	(if (negative? z) (%+ z y) z))))

(define-inline (abs x) 
  (if (negative? x) (%- x) x))

(define-inline (sin x) (%ieee754-sin x))
(define-inline (cos x) (%ieee754-cos x))
(define-inline (tan x) (%ieee754-tan x))

(define atan
  (let* ((pi (%ieee754-pi)) 
	 (pi/2 (%/ pi 2)))
    (case-lambda
      ((x) (%ieee754-atan1 x))
      ((y x)
       (let ((y (exact->inexact y)))
	 (cond ((%= x 0) (if (%> y 0) pi/2 (%- pi/2))) ; y == 0 -> undefined
	       ((%> x 0) (%ieee754-atan1 (%/ y x)))
	       ((%< y 0) (%- (%ieee754-atan1 (%/ y x)) pi))
	       (else (%+ (%ieee754-atan1 (%/ y x)) pi))))))))

(define-inline (asin x) (%ieee754-asin x))
(define-inline (acos x) (%ieee754-acos x))

(define-inline (log x) (%ieee754-log x))

(define-inline (sqrt x) (%ieee754-sqrt x))

(define-inline (expt x y)
  (if (%eq? y 0) 
      1
      (cond ((and (inexact? y) (%= y 0.0)) 1.0)
	    ((and (exact? x) (exact? y) (positive? y) (%fx<? y 256)) ($inline "CALL fixnum_expt" x y))
	    ((negative? y) (%/ 1 ($inline "CALL flonum_expt" x (%- y))))
	    (else ($inline "CALL flonum_expt" x y)))))

(let-syntax ((e 2.7182818284590452353602874))
  (define-inline (exp x) (expt e x)))

(define-inline (cons x y) ($allocate 2 2 x y))
(define-inline (length x) ($inline "CALL list_length" x))

(define gcd #f)
(define lcm #f)

(let-syntax ((abs 
	      (syntax-rules ()
		((_ x) (if (negative? x) (%- x) x))))
	     (rem
	      (syntax-rules ()
		((_ x y)
		 (let ((a x) (b y))
		   (%- a (%* (quotient a b) b)))))))
  (define (gcd1 x y)
    (let loop ((x x) (y y))
      (if (zero? y)
	  (abs x)
	  (loop y (rem x y)) ) ) )
  (define (lcm1 x y)
    (quotient (%* x y) (gcd1 x y)) )
  (set! gcd
    (lambda ns
      (if (null? ns)
	  0
	  (let loop ((ns ns) (f #t))
	    (let ((head (car ns))
		  (next (cdr ns)) )
	      (if (null? next)
		  (abs head)
		  (let ((n2 (car next)))
		    (loop (cons (gcd1 head n2) (cdr next)) #f) ) ) ) ) ) ))
  (set! lcm
    (lambda ns
      (if (null? ns)
	  1
	  (let loop ((ns ns) (f #t))
	    (let ((head (car ns))
		  (next (cdr ns)) )
	      (if (null? next)
		  (abs head)
		  (let ((n2 (car next)))
		    (loop (cons (lcm1 head n2) (cdr next)) #f) ) ) ) ) ) )))

(define (list? x)
  (let loop ((fast x) (slow x))
    (or (null? fast)
	(and (pair? fast)
	     (let ((fast (cdr fast)))
	       (or (null? fast)
		   (and (pair? fast)
			(let ((fast (cdr fast))
			      (slow (cdr slow)))
			  (and (not (%eq? fast slow))
			       (loop fast slow))))))))))

(define-inline (string-length s) (%size s))
(define-inline (vector-length v) (%size v))

(define (list-tail lst i)
  (let loop ((lst lst) (i i))
    (if (%eq? i 0)
	lst
	(loop (cdr lst) (%fx- i 1)))))

(define list-ref
  (let ((list-tail list-tail))
    (lambda (lst i)
      (car (list-tail lst i)))))

(define (reverse lst)
  (let loop ((lst lst) (rest '()))
    (if (pair? lst)
	(loop (cdr lst) (cons (car lst) rest))
	rest)))

(define (append . lsts)
  (if (null? lsts)
      '()
      (let loop ((lsts lsts))
	(if (null? (cdr lsts))
	    (car lsts)
	    (let copy ((node (car lsts)))
	      (if (pair? node)
		  (cons (car node) (copy (cdr node)))
		  (loop (cdr lsts))))))))

(define-inline (string-ref x i) (integer->char (%byte-ref x i)))
(define-inline (string-set! x i y) (%byte-set! x i (char->integer y)))

(define %apply ($primitive "apply"))

(define-syntax apply %apply)

(define-inline (%make-port in? fd close r/w data)
  (let ((p ($allocate 8 6 fd in?)))
    (%slot-set! p 2 close)
    (%slot-set! p 3 r/w)
    (%slot-set! p 4 #f)		; peek
    (%slot-set! p 5 data)
    p))


(define-inline (raise exn)
  (%current-exception-handler exn))

(define-syntax-rule (%make-error-object kind loc msg args)
  (let ((exn ($allocate 10 6 'error-object 1 msg)))
    (%slot-set! exn 3 args)
    (%slot-set! exn 4 loc)
    (%slot-set! exn 5 kind)
    exn))


(cond-expand
  ((or file-ports file-system)
   (define (%file-error loc . args)
     (%interrupt-hook)
     (raise (%make-error-object 'file loc (%errno-string) args))))
  (else))


(cond-expand
  (file-ports

   (define (%make-file-input-port fd)
     (%make-port 
      #t fd
      (lambda (p)
	(%slot-set! p 4 #f)
	(when (%fx<? (%close (%slot-ref p 0)) 0)
	  (%file-error 'close-input-port p)))
      (lambda (p n) 
	(let* ((str (%allocate-block #x11 n #f n #f #f))
	       (nr (%read str (%slot-ref p 0) n)))
	  (cond ((%eq? nr 0) (eof-object))
		((%eq? n nr) str)
		((%fx>? nr 0)
		 (let ((str2 (%allocate-block #x11 nr #f nr #f #f)))
		   ($inline "CALL copy_bytes" (cons str 0) (cons str2 0) nr)
		   str2))
		(else (%file-error 'read-string p n)))))
      #f))

   (define (%make-file-output-port fd)
     (%make-port 
      #f fd
      (lambda (p)
	(when (%fx<? (%close (%slot-ref p 0)) 0)
	  (%file-error 'close-output-port p)))
      (lambda (p str)
	(when (%fx<? (%write str (%slot-ref p 0) (string-length str)) 0)
	  (%file-error 'write-string p str)))
      #f))
       
   (define %standard-input-port (%make-file-input-port 0))
   (define %standard-output-port (%make-file-output-port 1))
   (define %standard-error-port (%make-file-output-port 2)))
  
  (else

   (define (%make-null-input-port)
     (%make-port
      #t #f
      (lambda (p) 0)
      (lambda (p n) (eof-object))
      #f))

   (define (%make-null-output-port)
     (%make-port
      #f #f
      (lambda (p) 0)
      (lambda (p n) 0)
      #f))

   (define %standard-input-port (%make-null-input-port))
   (define %standard-output-port (%make-null-output-port))
   (define %standard-error-port (%make-null-output-port))))
   

(define-syntax current-input-port
  (case-lambda
   (() %standard-input-port)
   ((p . r) (set! %standard-input-port p)))) ; hack for parameterize

(define-syntax current-output-port
  (case-lambda 
   (() %standard-output-port)
   ((p . r) (set! %standard-output-port p))))

(define-syntax current-error-port
  (case-lambda
   (() %standard-error-port)
   ((p . r) (set! %standard-error-port p))))

(cond-expand
  (file-ports

   (define (open-input-file name)
     (let ((fd (%open-input-file name)))
       (if (%fx<? fd 0)
	   (%file-error 'open-input-file name)
	   (%make-file-input-port fd))))

   (define (open-output-file name)
     (let ((fd (%open-output-file name)))
       (if (%fx<? fd 0)
	   (%file-error 'open-output-file name)
	   (%make-file-output-port fd)))))
  
  (else))

(define-inline (close-input-port p) ((%slot-ref p 2) p))
(define-inline (close-output-port p) ((%slot-ref p 2) p))

(define-inline (%char->string c)
  (let ((str ($allocate #x11 1)))
    (%byte-set! str 0 (char->integer c))
    str))

(define-syntax write-char
  (case-lambda
   ((c) ((%slot-ref %standard-output-port 3) %standard-output-port (%char->string c)))
   ((c p) ((%slot-ref p 3) p (%char->string c)))))

(define-syntax newline
  (case-lambda
    (() (write-char #\newline %standard-output-port))
    ((p) (write-char #\newline p))))

(define-inline (%read-char p)
  (let ((pc (%slot-ref p 4)))
    (cond (pc 
	   (%slot-set! p 4 #f)
	   pc)
	  (else
	   (let ((str ((%slot-ref p 3) p 1)))
	     (if (eof-object? str)
		 str
		 (string-ref str 0)))))))

(define-syntax read-char
  (case-lambda
   (() (%read-char %standard-input-port))
   ((p) (%read-char p))))

(define-inline (%peek-char p)
  (let ((pc (%slot-ref p 4)))
    (or pc
	(let ((str ((%slot-ref p 3) p 1)))
	  (if (eof-object? str)
	      str
	      (let ((c (string-ref str 0)))
		(%slot-set! p 4 c)
		c))))))

(define-syntax peek-char
  (case-lambda
   (() (%peek-char %standard-input-port))
   ((p) (%peek-char p))))

(define-inline (vector-ref x i) (%slot-ref x i))
(define-inline (vector-set! x i y) (%slot-set! x i y))

;; these don't cons up a character and just return a fixnum result
(define-inline (%char-downcase c) 
  (let ((n (char->integer c)))
    (if (or (%fx<? n #x41)		; A
	    (%fx>? n #x5a))		; Z
	n
	(bitwise-ior #x20 n))))

(define-inline (%char-upcase c) 
  (let ((n (char->integer c)))
    (if (or (%fx<? n #x61)		; a
	    (%fx>? n #x7a))		; z
	n
	(bitwise-and #xdf n))))

(define-inline (char-downcase c) (integer->char (%char-downcase c)))
(define-inline (char-upcase c) (integer->char (%char-upcase c)))

(define-inline (char-alphabetic? c)
  (let ((n (char->integer c)))
    (cond ((%fx<? n #x41) #f)		; A
	  ((%fx>? n #x7a) #f)		; z
	  ((%fx>? n #x60))		; a-1
	  ((%fx<? n #x5b))		; Z+1
	  (else #f))))

(define-inline (char-numeric? c)
  (let ((n (char->integer c)))
    (cond ((%fx<? n #x30) #f)		; 0
	  ((%fx>? n #x39) #f)		; 9
	  (else #t))))

(define-inline (char-whitespace? c)
  (let ((n (char->integer c)))
    (or (%eq? n 32) (%eq? n 9) (%eq? n 12) (%eq? n 10) (%eq? n 13))))

(define-inline (char-upper-case? c)
  (let ((n (char->integer c)))
    (cond ((%fx<? n #x41) #f)		; A
	  ((%fx>? n #x5a) #f)		; Z
	  (else #t))))

(define-inline (char-lower-case? c)
  (let ((n (char->integer c)))
    (cond ((%fx<? n #x61) #f)		; a
	  ((%fx>? n #x7a) #f)		; z
	  (else #t))))

(define-inline (char=? x y) (%eq? (%slot-ref x 0) (%slot-ref y 0)))
(define-inline (char>? x y) (%fx>? (%slot-ref x 0) (%slot-ref y 0)))
(define-inline (char<? x y) (%fx<? (%slot-ref x 0) (%slot-ref y 0)))
(define-inline (char>=? x y) (%fx>=? (%slot-ref x 0) (%slot-ref y 0)))
(define-inline (char<=? x y) (%fx<=? (%slot-ref x 0) (%slot-ref y 0)))

(define-inline (char-ci=? x y) (%eq? (%char-downcase x) (%char-downcase y)))
(define-inline (char-ci>? x y) (%fx>? (%char-downcase x) (%char-downcase y)))
(define-inline (char-ci<? x y) (%fx<? (%char-downcase x) (%char-downcase y)))
(define-inline (char-ci>=? x y) (%fx>=? (%char-downcase x) (%char-downcase y)))
(define-inline (char-ci<=? x y) (%fx<=? (%char-downcase x) (%char-downcase y)))

(define-syntax string=? eqv?)

(define-inline (string>? x y)
  (let* ((xlen (string-length x))
	 (ylen (string-length y))
	 (len (if (%fx<? xlen ylen) xlen ylen))
	 (r ($inline "CALL compare_strings" x y len)))
    (if (%eq? r 0)
	(%fx>? xlen ylen)
	(%eq? r 1))))

(define-inline (string<? x y)
  (let* ((xlen (string-length x))
	 (ylen (string-length y))
	 (len (if (%fx<? xlen ylen) xlen ylen))
	 (r ($inline "CALL compare_strings" x y len)))
    (if (%eq? r 0)
	(%fx<? xlen ylen)
	(%eq? r -1))))

(define-inline (string>=? x y)
  (let* ((xlen (string-length x))
	 (ylen (string-length y))
	 (len (if (%fx<? xlen ylen) xlen ylen))
	 (r ($inline "CALL compare_strings" x y len)))
    (if (%eq? r 0)
	(%fx>=? xlen ylen)
	(%fx>=? r 0))))

(define-inline (string<=? x y)
  (let* ((xlen (string-length x))
	 (ylen (string-length y))
	 (len (if (%fx<? xlen ylen) xlen ylen))
	 (r ($inline "CALL compare_strings" x y len)))
    (if (%eq? r 0)
	(%fx<=? xlen ylen)
	(%fx<=? r 0))))

(define-inline (string-ci=? x y)
  (let ((len (string-length x)))
    (and (%eq? len (string-length y))
	 (%eq? ($inline "CALL compare_strings_ci" x y len) 0))))

(define-inline (string-ci>? x y)
  (let* ((xlen (string-length x))
	 (ylen (string-length y))
	 (len (if (%fx<? xlen ylen) xlen ylen))
	 (r ($inline "CALL compare_strings_ci" x y len)))
    (if (%eq? r 0)
	(%fx>? xlen ylen)
	(%eq? r 1))))

(define-inline (string-ci<? x y)
  (let* ((xlen (string-length x))
	 (ylen (string-length y))
	 (len (if (%fx<? xlen ylen) xlen ylen))
	 (r ($inline "CALL compare_strings_ci" x y len)))
    (if (%eq? r 0)
	(%fx<? xlen ylen)
	(%eq? r -1))))

(define-inline (string-ci>=? x y)
  (let* ((xlen (string-length x))
	 (ylen (string-length y))
	 (len (if (%fx<? xlen ylen) xlen ylen))
	 (r ($inline "CALL compare_strings_ci" x y len)))
    (if (%eq? r 0)
	(%fx>=? xlen ylen)
	(%fx>=? r 0))))

(define-inline (string-ci<=? x y)
  (let* ((xlen (string-length x))
	 (ylen (string-length y))
	 (len (if (%fx<? xlen ylen) xlen ylen))
	 (r ($inline "CALL compare_strings_ci" x y len)))
    (if (%eq? r 0)
	(%fx<=? xlen ylen)
	(%fx<=? r 0))))

(define-inline (string-fill! s c)
  ($inline "CALL fill_bytes" s (char->integer c)))

(define-syntax make-string
  (case-lambda
   ((n) (%allocate-block #x11 n #f n #f #f))
   ((n c)
    (let ((str (%allocate-block #x11 n #f n #f #f)))
      (string-fill! str c)
      str))))

(define (string-append . slst)
  (let loop ((ss slst) (n 0))
    (if (null? ss)
	(let ((dest (cons (%allocate-block #x11 n #f n #f #f) 0)))
	  (let loop ((ss slst))
	    (if (null? ss)
		(car dest)
		(let* ((src (car ss))
		       (len (string-length src)))
		  ($inline "CALL copy_bytes" (cons src 0) dest len)
		  (set-cdr! dest (%fx+ (cdr dest) len))
		  (loop (cdr ss))))))
	(loop (cdr ss) (%fx+ n (string-length (car ss)))))))

(define-inline (string-copy str)
  (let* ((len (string-length str))
	 (str2 (%allocate-block #x11 len #f len #f #f)))
    ($inline "CALL copy_bytes" (cons str 0) (cons str2 0) len)
    str2))

(define-syntax substring
  (case-lambda
   ((str from)
    (let* ((to (string-length str))
	   (len (%fx- to from))
	   (str2 (%allocate-block #x11 len #f len #f #f)))
      ($inline "CALL copy_bytes" (cons str from) (cons str2 0) len)
      str2))
   ((str from to)
    (let* ((len (%fx- to from))
	   (str2 (%allocate-block #x11 len #f len #f #f)))
      ($inline "CALL copy_bytes" (cons str from) (cons str2 0) len)
      str2))))

(cond-expand 
  (nolibc

   (define (string->number str . base)
     (let* ((base (optional base 10))
	    (len (string-length str))
	    (s 1)
	    (p 0))
       (cond ((%eq? 0 len) #f)
	     (else
	      (case (string-ref str 0)
		((#\-) 
		 (set! s -1)
		 (set! p 1))
		((#\+)
		 (set! p 1)))
	      (and (%fx>? len p)
		   (let loop ((p p) (n 0))
		     (if (%fx>=? p len) 
			 (%fx* s n)
			 (let* ((c (char-downcase (string-ref str p)))
				(r (%fx- (char->integer c) (if (char>=? c #\a) 87 48))))
			   (and (%fx>=? r 0) (%fx<? r base)
				(loop (%fx+ p 1) (%fx+ (%fx* n base) r)))))))))))

   (let-syntax ((buflen 100))
     (define number->string
       (let ((buffer (make-string buflen)))
	 (lambda (num . base)
	   (cond ((inexact? num) (%error "sorry, float->string conversion is not implemented in `nolibc' mode"))
		 ((%eq? num 0) "0")
		 (else
		  (let ((neg (negative? num))
			(base (optional base 10)))
		    (let loop ((p buflen) (n (if neg (%fx- 0 num) num)))
		      (cond ((%eq? n 0)
			     (when neg
			       (set! p (%fx- p 1))
			       (string-set! buffer p #\-))
			     (substring buffer p))
			    (else
			     (%fx-divmod 
			      n base
			      (lambda (q r)
				(let ((p (%fx- p 1)))
				  (string-set! 
				   buffer p
				   (integer->char (%fx+ (if (%fx>=? r 10) 87 48) r)))
				  (loop p q)))))))))))))))

  (else

   (define (%string->number str base)
     (let ((len (string-length str)))
       (cond ((%eq? len 0) #f)
	     ((or (string-ci=? str "+nan.0")
		  (string-ci=? str "-nan.0"))
	      (%ieee754-nan))
	     ((string-ci=? str "+inf.0")
	      (%ieee754-infinity))
	     ((string-ci=? str "-inf.0")
	      (%ieee754-negative-infinity))
	     (else
	      (let ((n ($inline "CALL str2num" str base)))
		(and (finite? n) n)))))) ; handle "[-+]infinity"
   
   (define-syntax string->number
     (case-lambda 
       ((str base) (%string->number str base))
       ((str) (%string->number str 10))))

   (define (number->string num . base)
     (cond ((nan? num) "+nan.0")
	   ((finite? num)
	    (let ((str ($inline "CALL num2str" num (optional base 10))))
	      (if (and (inexact? num) (integer? num))
		  (string-append str ".0")
		  str)))
	   ((negative? num) "-inf.0")
	   (else "+inf.0")))))


(define-inline (vector-fill! v x)
  ($inline "CALL fill_slots" v x))

(define-inline (vector-copy vec)
  (let* ((len (vector-length vec))
	 (vec2 (%allocate-block 3 (%cells len) #f len #t #f)))
    ($inline "CALL copy_slots" (cons vec 0) (cons vec2 0) len)
    vec2))

(define-syntax make-vector
  (case-lambda 
   ((n) (%allocate-block 3 (arithmetic-shift n %cell-shift) #f n #t (%undefined)))
   ((n x) (%allocate-block 3 (arithmetic-shift n %cell-shift) #f n #t x))))

(define (list->vector lst) ;XXX this can probably be done more efficiently
  (let* ((n (length lst))
	 (v (%allocate-block 3 (%cells n) #f n #t #f)))
    (do ((i 0 (%fx+ i 1))
	 (lst lst (cdr lst)))
	((%fx>=? i n) v)
      (vector-set! v i (car lst)))))

(define (vector->list vec)
  (let ((n (vector-length vec)))
    (let loop ((i (%fx- n 1)) (lst '()))
      (if (%fx<? i 0)
	  lst
	  (loop (%fx- i 1) (cons (vector-ref vec i) lst))))))

(define (list->string lst)
  (let* ((n (length lst))
	 (s (%allocate-block #x11 n #f n #f #f)))
    (do ((i 0 (%fx+ i 1))
	 (lst lst (cdr lst)))
	((%fx>=? i n) s)
      (string-set! s i (car lst)))))

(define (string->list str)
  (let ((n (string-length str)))
    (let loop ((i (%fx- n 1)) (lst '()))
      (if (%fx<? i 0)
	  lst
	  (loop (%fx- i 1) (cons (string-ref str i) lst))))))

(define (vector . xs) (list->vector xs))
(define (string . cs) (list->string cs))

(define-inline (symbol->string x) (string-copy (%slot-ref x 0)))

(define %symbol-table (make-vector 256 '()))

(define-inline (%hash str) ($inline "CALL hash_string" str))

(define (string->symbol str)
  (let* ((i (%hash str))
	 (b (vector-ref %symbol-table i)))
    (let loop ((bucket b))
      (cond ((null? bucket)
	     (let ((sym ($allocate 1 1 (string-copy str))))
	       (vector-set! %symbol-table i (cons sym b))
	       sym))
	    ((string=? str (%slot-ref (car bucket) 0)) (car bucket))
	    (else (loop (cdr bucket)))))))

(let loop ((i 0))
  (let ((sl (%symbol-literal i)))
    (when sl
      (let ((h (%hash (%slot-ref sl 0))))
	(vector-set! %symbol-table h (cons sl (vector-ref %symbol-table h)))
	(loop (%fx+ i 1))))))

(define (for-each proc lst1 . lsts)
  (if (null? lsts)
      (let loop ((lst lst1))		; fast case
	(unless (null? lst)
	  (proc (car lst))
	  (loop (cdr lst))))
      (let loop ((lsts (cons lst1 lsts)))
	(let ((hds (let loop2 ((lsts lsts))
		     (if (null? lsts)
			 '()
			 (let ((x (car lsts)))
			   (and (not (null? x))
				(let ((r (loop2 (cdr lsts))))
				  (and r (cons (car x) r)))))))))
	  (when hds
	    (%apply proc hds)
	    (loop
	     (let loop3 ((lsts lsts))
	       (if (null? lsts)
		   '()
		   (cons (cdar lsts) (loop3 (cdr lsts)))))))))))

(define (map proc lst1 . lsts)
  (if (null? lsts)
      (let loop ((lst lst1))		; fast case
	(if (null? lst)
	    '()
	    (cons (proc (car lst)) (loop (cdr lst)))))
      (let loop ((lsts (cons lst1 lsts)))
	(let ((hds (let loop2 ((lsts lsts))
		     (if (null? lsts)
			 '()
			 (let ((x (car lsts)))
			   (and (not (null? x))
				(let ((r (loop2 (cdr lsts))))
				  (and r (cons (car x) r)))))))))
	  (if hds
	      (cons
	       (%apply proc hds)
	       (loop
		(let loop3 ((lsts lsts))
		  (if (null? lsts)
		      '()
		      (cons (cdar lsts) (loop3 (cdr lsts)))))))
	      '())))))

(define-inline (assq x lst) ($inline "CALL assoc_eq" x lst))
(define-inline (assv x lst) ($inline "CALL assoc_eqv" x lst))
(define-inline (assoc x lst) ($inline "CALL assoc_equal" x lst))

(define-inline (memq x lst) ($inline "CALL member_eq" x lst))
(define-inline (memv x lst) ($inline "CALL member_eqv" x lst))
(define-inline (member x lst) ($inline "CALL member_equal" x lst))

(define values ($primitive "values"))
(define call-with-values ($primitive "call_with_values"))
(define %call/cc ($primitive "call_cc")) ; non-winding

(define %dynamic-winds '())

(define (dynamic-wind before thunk after)
  (before)
  (set! %dynamic-winds (cons (cons before after) %dynamic-winds))
  (call-with-values thunk
    (lambda results
      (set! %dynamic-winds (cdr %dynamic-winds))
      (after)
      (%apply values results))))

(define (%dynamic-unwind topitems n)
  (cond ((%eq? topitems %dynamic-winds))
	((%fx<? n 0)
	 (%dynamic-unwind (cdr topitems) (%fx+ n 1))
	 ((caar %dynamic-winds))
	 (set! %dynamic-winds topitems))
	(else
	 (let ((after (cdar %dynamic-winds)))
	   (set! %dynamic-winds (cdr %dynamic-winds))
	   (after)
	   (%dynamic-unwind topitems (%fx- n 1))))))

(define (call-with-current-continuation proc)
  (let ((topitems %dynamic-winds))
    (%call/cc
     (lambda (cont)
       (proc
	(lambda results
	  (let ((items2 %dynamic-winds))
	    (unless (%eq? items2 topitems)
	      (%dynamic-unwind topitems (%fx- (length items2) (length topitems))) )
	    (%apply cont results) ) ) ) ) ) ))


(let-syntax ((write-string 
	      (syntax-rules ()
		((_ s p) ((%slot-ref p 3) p s)))))
  (define (%output-to-port x rd port)
    (define (escape-symbol? str) ; look for symbol-characters to be escaped
      (let ((len (string-length str)))
	(let loop ((i 0))
	  (if (%fx>=? i len)
	      #f
	      (let ((c (string-ref str i)))
		(or (char=? c #\|) (char=? c #\\) (char=? c #\() (char=? c #\))
		    (char=? c #\;) (char=? c #\#) (char=? c #\") (char=? c #\[)
		    (char=? c #\]) (char=? c #\{) (char=? c #\})
		    (%fx<=? (char->integer c) 32)
		    (loop (%fx+ i 1))))))))
    (letrec-syntax ((outs (syntax-rules ()
			    ((_ s) (write-string s port))))
		    (out (syntax-rules ()
			   ((_ c) (outs (%char->string c))))))
      (let show ((x x))
	(cond ((vector? x)
	       (let ((len (vector-length x)))
		 (outs "#")
		 (show (vector->list x))))
	      ((pair? x)
	       (outs "(")
	       (show (car x))
	       (let loop ((x (cdr x)))
		 (cond ((null? x) (outs ")"))
		       ((pair? x) 
			(outs " ")
			(show (car x))
			(loop (cdr x)))
		       (else 
			(outs " . ")
			(show x)
			(outs ")")))))
	      ((string? x) 
	       (cond (rd
		      (outs "\"")
		      (let ((len (string-length x)))
			(do ((i 0 (%fx+ i 1)))
			    ((%fx>=? i len) (outs "\""))
			  (let* ((c (string-ref x i))
				 (n (char->integer c)))
			    (cond ((or (char=? c #\\) (char=? c #\"))
				   (outs "\\")
				   (out c))
				  ((char=? c #\newline)
				   (outs "\\n"))
			          ((%fx<? n 32)
				   (outs "\\x")
				   (outs (number->string n 16))
				   (outs ";"))
				  (else (out c)))))))
		     (else (outs x))))
	      ((symbol? x)
	       (let ((str (%slot-ref x 0))) ; avoid copying
		 (cond ((and rd (escape-symbol? str))
			(outs "|")
			(let* ((str str)
			       (len (string-length str)))
			  (do ((i 0 (%fx+ i 1)))
			      ((%fx>=? i len)
			       (out "|"))
			    (let* ((c (string-ref str i))
				   (n (char->integer c)))
			      (cond ((or (char=? c #\\) (char=? c #\|))
				     (outs "\\")
				     (out c))
				    ((%fx<? n 32)
				     (outs "\\x")
				     (outs (number->string n 16))
				     (outs ";"))
				    (else (out c)))))))
		       (else (outs str)))))
	      ((char? x) 
	       (cond (rd
		      (outs "#\\")
		      (case x
			((#\newline) (outs "newline"))
			((#\space) (outs "space"))
			((#\tab) (outs "tab"))
			(else
			 (let ((n (char->integer x)))
			   (cond ((and (%fx>=? n 0) (%fx<? n 256))
				  (out x))
				 (else
				  (outs "x")
				  (outs (number->string n 16))))))))
		     (else (out x))))
	      ((number? x) (outs (number->string x)))
	      ((null? x) (outs "()"))
	      ((promise? x) (outs "#<promise>"))
	      ((record? x)
	       (outs (string-append 
		      "#<record "
		      (symbol->string (%slot-ref x 0)) ; record-type name-symbol
		      "/"
		      (number->string (%slot-ref x 1)) ; record-type id
		      ">")))
	      ((bytevector? x)
	       (outs "#u8(")
	       (let ((len (%size x)))
		 (when (%fx>? len 0)
		   (show (%byte-ref x 0))
		   (do ((i 1 (%fx+ i 1)))
		       ((%fx>=? i len))
		     (out #\space)
		     (show (%byte-ref x i))))
		 (out #\))))
	      ((input-port? x) (outs "#<input-port>"))
	      ((output-port? x) (outs "#<output-port>"))
	      ((procedure? x) (outs "#<procedure>"))
	      ((eof-object? x) (outs "#<eof>"))
	      ((%eq? (%undefined) x) (outs "#<undefined>"))
	      ((%eq? #t x) (outs "#t"))
	      ((%eq? #f x) (outs "#f"))
	      (else (outs "#<unknown object>")))))))

(define-syntax display
  (case-lambda
    ((x) (%output-to-port x #f %standard-output-port))
    ((x p) (%output-to-port x #f p))))

(define-syntax write
  (case-lambda
    ((x) (%output-to-port x #t %standard-output-port))
    ((x p) (%output-to-port x #t p))))

(define (emergency-exit . code)
  (let ((code (optional code 0)))
    (when (boolean? code)
      (set! code (if code 0 70)))	; EXIT_FAILURE
    (%_exit code)))

(define (exit . code)
  (let ((code (optional code 0)))
    (when (boolean? code)
      (set! code (if code 0 70)))
    (let loop ()
      (cond ((null? %dynamic-winds) (%exit code))
	    (else
	     (let ((dw (car %dynamic-winds)))
	       (set! %dynamic-winds (cdr %dynamic-winds))
	       ((cdr dw))
	       (loop)))))))

(cond-expand
  (embedded
   (define return-to-host ($primitive "return_to_host")))
  (else))


(define-inline (error-object? x) 
  (and (record? x) (%eq? (%slot-ref x 1) 1)))

(define-inline (error-object-message exn) (%slot-ref exn 2))
(define-inline (error-object-irritants exn) (%slot-ref exn 3))
(define-inline (error-object-location exn) (%slot-ref exn 4))

(define (%current-exception-handler exn)
  (cond-expand
    (embedded (return-to-host exn))
    (else
     (let* ((exn? (error-object? exn))
	    (msg (if exn? (error-object-message exn) "unhandled exception"))
	    (loc (and exn? (error-object-location exn)))
	    (args (if exn? (error-object-irritants exn) (list exn))))
       (display "\nError: " %standard-error-port)
       (when loc
	 (display (string-append "(" (%slot-ref loc 0) ") ") %standard-error-port))
       (display (string-append msg "\n") %standard-error-port)
       (unless (null? args)
	 (for-each
	  (lambda (arg)
	    (newline %standard-error-port)
	    (write arg)
	    (newline %standard-error-port))
	  args))
       (emergency-exit 70)))))      	; EXIT_FAILURE

(define (%error msg . args)
  (let ((loc #f))
    (cond ((and (symbol? msg) (pair? args) (string? (car args)))
	   (set! loc msg)
	   (set! msg (car args))
	   (set! args (cdr args)))
	  ((and (not (string? msg)) (pair? args))
	   (set! msg "unknown error")
	   (set! args (cons msg args))))
    (raise
     (if (string? msg)
	 (%make-error-object #f loc msg args)
	 msg))))

(define (%interrupt-hook)
  (let ((n (%check-interrupts)))
    (unless (%eq? n 0)
      (let loop ((m 1) (i 1))
	(cond ((%eq? 0 n))
	      ((%eq? 0 (bitwise-and n m))
	       (loop (arithmetic-shift m 1) (%fx+ i 1)))
	      (else
	       (%clear-interrupt i)
	       (raise (%make-error-object 'interrupt #f "interrupt" (list i)))))))))

(define-syntax error %error)

(define-syntax current-exception-handler
  (case-lambda
    (() %current-exception-handler)
    ((xh . r) (set! %current-exception-handler xh)))) ; fake parameter

(define-inline (file-error? x)
  (and (error-object? x) (%eq? 'file (%slot-ref x 5))))

(define-inline (read-error? x)
  (and (error-object? x) (%eq? 'read (%slot-ref x 5))))

(define-inline (interrupt? x)
  (and (error-object? x) (%eq? 'interrupt (%slot-ref x 5))))

(define-inline (interrupt-number x)
  (car (error-object-irritants x)))

(define-inline (check-interrupts) (%interrupt-hook))

(define-syntax catch-interrupt
  (case-lambda
    (() (%sigaction %SIGINT 0))
    ((num) (%sigaction num 0))
    ((num mode) (%sigaction %SIGINT mode))))


(define case-sensitive
  (let ((cs #t))
    (lambda arg
      (if (null? arg)
	  cs
	  (set! cs (car arg))))))

(define (read . p)
  (define (read-error msg . args)
    (raise (%make-error-object 'read 'read msg args)))
  (let ((port (optional p %standard-input-port))
	(cs (case-sensitive))
	(eol (lambda (c) (read-error "unexpected delimiter" c))))
    (define (parse-token t)
      (or (string->number t)
	  (string->symbol t)))
    (define (read1)
      (let ((c (read-char port)))
	(if (eof-object? c) 
	    c
	    (case c
	      ((#\#) (read-sharp))
	      ((#\() (read-list #\)))
	      ((#\[) (read-list #\]))
	      ((#\{) (read-list #\}))
	      ((#\,)
	       (cond ((eqv? (peek-char port) #\@)
		      (read-char port)
		      (%list 'unquote-splicing (read1)))
		     (else (%list 'unquote (read1)))))
	      ((#\`) (%list 'quasiquote (read1)))
	      ((#\') `',(read1))
	      ((#\;) (skip-line) (read1))
	      ((#\") (read-string))
	      ((#\|) (string->symbol (read-delimited #\|)))
	      ((#\) #\] #\}) (eol c))
	      (else
	       (if (char-whitespace? c)
		   (read1)
		   (parse-token (read-token (%list (docase c)) cs))))))))
    (define (skip-line)
      (let ((c (read-char port)))
	(unless (or (eof-object? c) (char=? #\newline c))
	  (skip-line))))
    (define (skip-whitespace)		; returns peeked char
      (let ((c (peek-char port)))
	(cond ((eof-object? c) c)
	      ((char-whitespace? c)
	       (read-char port)
	       (skip-whitespace))
	      (else c))))
    (define (read-sharp)
      (let ((c (read-char port)))
	(if (eof-object? c)
	    (read-error "unexpected EOF after `#'")
	    (case c
	      ((#\f #\F) #f)
	      ((#\t #\T) #t)
	      ((#\x #\X) (string->number (read-token '() #f) 16))
	      ((#\o #\O) (string->number (read-token '() #f) 8))
	      ((#\b #\B) (string->number (read-token '() #f) 2))
	      ((#\i #\I) 
	       (let* ((tok (read-token '() #f))
		      (n (string->number tok)))
		 (if (not (number? n))
		     (read-error "invalid number syntax" tok)
		     (if (inexact? n) 
			 n
			 (exact->inexact n)))))
	      ((#\e #\E) 
	       (let* ((tok (read-token '() #f))
		      (n (string->number tok)))
		 (if (not (number? n))
		     (read-error "invalid number syntax" tok)
		     (if (exact? n) 
			 n
			 (inexact->exact n)))))
	      ((#\u #\U)
	       (let ((tok (read-token '() #f)))
		 (cond ((string=? tok "8")
			(read-char port) ; skip initial "(" (must come directly after "u8"!)
			(let* ((lst (read-list #\)))
			       (len (length lst))
			       (bv (%allocate-block #x12 len #f len #f #f)))
			  (do ((i 0 (%fx+ i 1))
			       (lst lst (cdr lst)))
			      ((%fx>=? i len))
			    (let ((elt (car lst)))
			      (unless (and (exact? elt) (%fx>=? elt 0) (%fx<? elt 256))
				(read-error "invalid element in bytevector" elt))
			      (%byte-set! bv i (car lst))))
			  bv))
		       (else
			(read-error "invalid read syntax" (string-append "#" (string c) tok))))))
	      ((#\() (list->vector (read-list #\))))
	      ((#\;) (read1) (read1))
	      ((#\%) (string->symbol (read-token (%list (docase c) #\#) cs)))
	      ((#\!) (skip-line) (read1))
	      ((#\\) 
	       (let* ((t (read-token '() #t))
		      (len (string-length t)))
		 (cond ((string-ci=? "newline" t) #\newline)
		       ((string-ci=? "tab" t) #\tab)
		       ((string-ci=? "space" t) #\space)
		       ((string-ci=? "return" t) #\return)
		       ((%eq? 0 len) (read-char port))
		       ((and (char-ci=? #\x (string-ref t 0))
			     (%fx>? len 1))
			(integer->char
			 (or (string->number (substring t 1) 16)
			     (read-error "invalid character name" t))))
		       (else (string-ref t 0)))))
	      ((#\') `(syntax ,(read1))) ; for... whatever
	      (else (read-error "invalid `#' syntax" c))))))
    (define (read-list delim)
      (call-with-current-continuation
       (lambda (return)
	 (let ((lst '())
	       (old eol))
	   (set! eol
	     (lambda (c)
	       (set! eol old)
	       (if (eqv? c delim)
		   (return (reverse lst))
		   (read-error "missing closing delimiter" delim))))
	   (let loop ()
	     (let ((c (skip-whitespace)))
	       (cond ((eof-object? c)
		      (read-error "unexpected EOF while reading list"))
		     ((char=? c delim)
		      (read-char port)
		      (set! eol old)
		      (return (reverse lst)))
		     (else
		      (if (eqv? #\. c)
			  (let ((t (read-token '() cs)))
			    (if (string=? "." t)
				(let ((rest (read1)))
				  (skip-whitespace)
				  (set! eol old)
				  (if (eqv? (read-char port) delim)
				      (return (append (reverse lst) rest))
				      (read-error "missing closing delimiter" delim)))
				(set! lst (cons (parse-token t) lst))))
			  (set! lst (cons (read1) lst)))
		      (loop)))))))))
    (define (read-delimited delim)
      (let loop ((lst '()))
	(let ((c (read-char port)))
	  (cond ((eof-object? c)
		 (read-error "unexpected EOF while reading delimited token"))
		((char=? delim c) 
		 (list->string (reverse lst)))
		((char=? #\\ c)
		 (let ((c (read-char port)))
		   (if (eof-object? c)
		       (read-error "unexpected EOF while reading delimited token")
		       (case c
			 ((#\n) (loop (cons #\newline lst)))
			 ((#\a) (loop (cons (integer->char 9) lst)))
			 ((#\r) (loop (cons #\return lst)))
			 ((#\t) (loop (cons #\tab lst)))
			 ((#\x)
			  (let loop2 ((v 0) (i 0))
			    (let ((c (read-char port)))
			      (cond ((eof-object? c)
				     (read-error "unexpected EOF while reading delimited token"))
				    ((char=? #\; c) 
				     (loop (cons (integer->char v) lst)))
				    ((and (char>=? c #\0) (char<=? c #\9))
				     (loop2 (%fx+ (arithmetic-shift v 4) (%fx- (char->integer c) 48)) (%fx+ i 1)))
				    ((and (char>=? c #\a) (char<=? c #\f))
				     (loop2 (%fx+ (arithmetic-shift v 4) (%fx- (char->integer c) 87)) (%fx+ i 1)))
				    ((and (char>=? c #\A) (char<=? c #\F))
				     (loop2 (%fx+ (arithmetic-shift v 4) (%fx- (char->integer c) 55)) (%fx+ i 1)))
				    (else (read-error "invalid escaped hexadecimal character in delimited token" c))))))
			 (else (loop (cons c lst)))))))
		(else (loop (cons c lst)))))))
    (define (read-string) (read-delimited #\"))
    (define (docase c)
      (if cs
	  c
	  (char-downcase c)))
    (define (read-token prefix cs)
      (let loop ((lst prefix))	     ; prefix must be in reverse order
	(let ((c (peek-char port)))
	  (if (or (eof-object? c)
		  (char-whitespace? c)
		  (memv c '(#\{ #\} #\( #\) #\[ #\] #\; #\")))
	      (list->string (reverse lst))
	      (let ((c (read-char port)))
		(loop (cons (if cs c (docase c)) lst)))))))
    (read1)))

(define (call-with-output-file fname proc)
  (let ((in (open-output-file fname)))
    (call-with-values (lambda () (proc in))
      (lambda results
	(close-output-port in)
	(%apply values results)))))

(define (call-with-input-file fname proc)
  (let ((in (open-input-file fname)))
    (call-with-values (lambda () (proc in))
      (lambda results
	(close-input-port in)
	(%apply values results)))))

(define (with-input-from-file fname thunk)
  (call-with-input-file fname
    (lambda (in)
      (let ((old %standard-input-port))
	(dynamic-wind
	    (lambda () (set! %standard-input-port in))
	    thunk
	    (lambda () (set! %standard-input-port old)))))))

(define (with-output-to-file fname thunk)
  (call-with-output-file fname
    (lambda (in)
      (let ((old %standard-output-port))
	(dynamic-wind
	    (lambda () (set! %standard-output-port in))
	    thunk
	    (lambda () (set! %standard-output-port old)))))))

(define (%make-promise thunk)
  (let ((ready #f)
	(results #f))
    ($allocate 
     9 1 
     (lambda ()
       (if ready
	   (apply values results)
	   (call-with-values thunk
	     (lambda xs
	       (cond (ready (apply values results))
		     (else
		      (set! ready #t)
		      (set! results xs)
		      (apply values results))))))))))

(define-inline (force p)
  (if (promise? p) 
      ((%slot-ref p 0))
      p))

(define-inline (truncate n)
  (if (exact? n)
      n
      (let ((e (%ieee754-exponent n)))
	(cond ((%fx>=? e 1075) n)	; no fractional part
	      ((%fx<? e 1023) 0.0)	; no integral part
	      (else
	       (let* ((m (%ieee754-mantissa n))
		      (mask (arithmetic-shift #xfffffffffffff (%fx- 0 (%fx- e 1023)))))
		 (if (%eq? 0 (bitwise-and m mask))
		     n
		     (%ieee754-mask n (bitwise-not mask)))))))))

(define-inline (ceiling n)
  (if (exact? n)
      n
      (let ((e (%ieee754-exponent n)))
	(cond ((%fx>=? e 1075) n)	; no fractional part
	      ((%fx<? e 1023)		; no integral part
	       (if (%eq? 0 (%ieee754-sign n))
		   1.0
		   0.0))
	      (else
	       (let* ((m (%ieee754-mantissa n))
		      (mask (arithmetic-shift #xfffffffffffff (%fx- 0 (%fx- e 1023)))))
		 (if (%eq? 0 (bitwise-and m mask))
		     1.0
		     (let ((n2 (%ieee754-mask n (bitwise-not mask))))
		       (if (%eq? 0 (%ieee754-sign n))
			   (%+ 1.0 n2)
			   n2)))))))))

(define-inline (floor n)
  (if (exact? n)
      n
      (let ((e (%ieee754-exponent n)))
	(cond ((%fx>=? e 1075) n)	; no fractional part
	      ((%fx<? e 1023)		; no integral part
	       (if (%eq? 0 (%ieee754-sign n))
		   0.0
		   -1.0))
	      (else
	       (let* ((m (%ieee754-mantissa n))
		      (mask (arithmetic-shift #xfffffffffffff (%fx- 0 (%fx- e 1023)))))
		 (if (%eq? 0 (bitwise-and m mask))
		     0.0
		     (let ((n2 (%ieee754-mask n (bitwise-not mask))))
		       (if (%eq? 0 (%ieee754-sign n))
			   n2
			   (%- (%- 1.0 n2)))))))))))

(define-inline (round n)
  (if (exact? n)
      n
      (%ieee754-round n)))
