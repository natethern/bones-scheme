;;;; bare bones scheme compiler


(define basic-implementation-features '(bones srfi-0 srfi-6 srfi-7 srfi-46))

(define argument-register-count 0)     ; set later
(define primitives '())
(define closures-to-be-translated '())
(define literals-to-be-translated '())
(define string-literals '())
(define symbol-table '())
(define label-counter 0)
(define allocating #f)
(define emit-expr-comments #f)

(define environment '())
(define locals-counter 0)
(define available-registers '())

(define (cells n) (* word-size n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (compile code . options)
  (set! lambda-id-counter 0)
  (set! argument-register-count (sub1 (length argument-registers)))
  (set! implementation-features
    (append (map string->symbol (collect-options 'feature: options))
	    (filter id (list target-os target-arch target-endianness))
	    basic-implementation-features))
  (set! file-search-path
    (append (collect-options 'library-path: options) '(".")))
  (let ((prg (match code
	       (('begin ('program . _))
		(expand-program (cadr code)))
	       (_ (if (option 'nostdlib: options)
		      code
		      (expand-program
		       `(program (include "base.scm") (code ,code))))))))
    (call/cc
     (lambda (return)
       (define (stop) (return #f))
       (when (option 'dump-features: options)
	 (for-each print implementation-features)
	 (stop))
       (expand-syntax (generate-cond-expand implementation-features))
       (let* ((code (expand-syntax prg))
	      (_ (when (option 'expand: options) 
		   (pp code)
		   (stop)))
	      (dumpcc (option 'dump-cc: options))
	      (dumpcps (option 'dump-cps: options))
	      (dumpserial (not (option 'dump-nested: options)))
	      (dumpcompiled (option 'dump: options))
	      (outfile (option 'output-file: options))
	      (code (canonicalize-expression code))
	      (defs code (cps code))
	      (_ (when dumpcps
		   (dump-expressions code dumpserial)
		   (stop)))
	      (code (detect-unused-variables code))
	      (_ (when dumpcompiled
		   (dump-expressions code dumpserial)
		   (stop)))
	      (ccode (cc code '())))
	 (set! emit-expr-comments (option 'comment: options))
	 (when dumpcc
	   (dump-expressions ccode dumpserial)
	   (stop))
	 ;;XXX add pass that assigns closure-id's to target variables, for adding comments in
	 ;;    generated output.
	 ((if outfile
	      (lambda (thunk)
		(with-output-to-file outfile thunk))
	      (lambda (thunk) (thunk)))
	  (cut generate-code defs ccode)))))))

(define (compile-file fname . options)
  (apply compile (read-forms fname) options))

(define (generate-code defs code)
  (set! label-counter 0)
  (generate-header)
  (set! literals-to-be-translated '())
  (set! primitives '())
  (generate-closures code)
  (generate-globals defs)
  (generate-literals)
  (generate-primitives)
  (generate-trailer))

(define (fixnum? n)
  (and (number? n) (exact? n) (<= (car fixnum-range) n (cdr fixnum-range))))

(define (register-literal c)
  (let ((l1 (label)))
    (push! (cons l1 c) literals-to-be-translated)
    l1))

(define (label)
  (string-append "L" (number->string (inc! label-counter))))

(define (simple-expression? exp)
  (match exp
    ;;XXX $allocate?
    ((or ('quote _)
	 '($undefined)
	 '($uninitialized)
	 ('$closure-ref _)
	 ('$box-ref (? simple-expression?))
	 ('$global-ref _)
	 ('$local-ref _))
     #t)
    (_ #f)))

(define (translate-inline-arguments args)
  (translate/registers args temporary-registers))

(define (blocked-register? reg)
  (not (memq reg available-registers)))

(define (translate x t)
  ;;(pp (if (pair? x) (car x) x))
  (when emit-expr-comments
    (generate-expr-comment (fragment x 4)))
  (match x
    (('$closure id cap . _)
     (push! x closures-to-be-translated)
     (set! allocating #t)
     (generate-closure-alloc (length cap) id)
     (do ((lst cap (cdr lst))
	  (off 2 (add1 off)))
	 ((null? lst))
       (translate (car lst) arg-register)
       (generate-slot-store alloc-register (cells off) arg-register))
     (generate-move t alloc-register)
     (generate-add alloc-register (cells (+ 2 (length cap))))
     #t)
    (('let ((vars vals) ...) body)
     (fluid-let ((environment environment)
		 (locals-counter locals-counter)
		 (available-registers available-registers))
       (let ((newenv environment))
	 (for-each
	  (lambda (var val)
	    (cond ((eq? var '$unused)
		   ;; drop if simple or just evaluate but don't bind
		   (unless (simple-expression? val)
		     (translate val arg-register)))
		  ((null? available-registers)
		   ;; evaluate and move into local
		   (translate val arg-register)
		   (generate-comment var " = local #" locals-counter)
		   (generate-move-to-local (cells locals-counter) arg-register)
		   (push! (cons var locals-counter) newenv)
		   (inc! locals-counter))
		  (else
		   ;; evaluate into target register
		   (let ((reg (car available-registers)))
		     (translate val arg-register)
		     (generate-comment var " = " reg)
		     (generate-move reg arg-register)
		     (push! (cons var reg) newenv)
		     (pop! available-registers)))))
	  vars vals)
	 (set! environment newenv)
	 (translate body t))))
    (('$global-set! var val)
     (translate val t)
     (generate-global-store var (mangle-identifier var) t)
     #t)
    (('$global-ref var)
     (generate-global-ref t var (mangle-identifier var))
     #t)
    (('$local-ref var)
     (let ((ref (lookup-variable var)))
       (if (symbol? ref)
	   (generate-move t ref)
	   (generate-local-ref t var ref)))
     #t)
    (('$local-set! var val)
     (translate val t)
     (let ((ref (lookup-variable var)))
       (if (symbol? ref)
	   (generate-move ref t)
	   (generate-local-store var ref t)))
     #t)
    (('if x y z)
     (translate x t)
     (let ((l1 (label))
	   (l2 (label)))
       (generate-conditional-branch t l1)
       (when (translate y t) ; ret-flag must be the same for both branches
	 (generate-jump l2))
       (emit l1 ":\n")
       (let ((ret (translate z t)))
	 (emit l2 ":\n")
	 ret)))
    (('$primitive (or ('quote name) name))
     (let ((l1 (label)))
       (push! (cons l1 name) primitives)
       (generate-immediate-ref t l1 name)
       #t))
    (('$box val)
     (translate val t)
     (generate-slot-store alloc-register (cells 1) t)
     (generate-immediate-ref t "VECTOR | 1")
     (generate-slot-store alloc-register 0 t)
     (generate-move t alloc-register)
     (generate-add alloc-register (cells 2))
     #t)
    (('$box-ref val)
     (translate val t)
     (generate-slot-ref t t (cells 1))
     #t)
    (('$box-set! box val)
     (match-let ((((_ . r1) (_ . r2)) (translate-inline-arguments (list box val))))
       (generate-slot-store r1 (cells 1) r2)
       (generate-move t r2)
       #t))
    (('$inline (or ('quote opr) opr) args ...)
     (assert (<= (length args) (length temporary-registers)) 
	     "too many arguments to `$inline'" args)
     (translate-inline-arguments args)
     (for-each
      (cut emit " " <> "\n")
      (string-split opr ";"))
     (generate-move t arg-register)
     #t)
    (('$allocate (or ('quote type) type) (or ('quote size) size) args ...)
     (assert (<= (length args) (length temporary-registers))
	     "too many arguments to `$allocate'" args)
     (let ((regs (translate-inline-arguments args))
	   (bytevec (not (zero? (bitwise-and type #x10)))))
       (set! allocating #t)
       (do ((regs regs (cdr regs))
	    (off 1 (add1 off)))
	   ((null? regs))
	 (generate-slot-store alloc-register (cells off) (cdar regs)))
       (generate-immediate-ref
	t
	(bitwise-ior (arithmetic-shift type (* (sub1 word-size) 8)) size)
	type "/" size)
       (generate-slot-store alloc-register 0 t)
       (generate-move t alloc-register)
       (generate-add 
	alloc-register
	(if bytevec
	    (string-append (number->string (cells 1)) " + ALIGNED(" (number->string size) ")")
	    (string-append (number->string (cells (add1 size))))))
       #t))
    (((or '$undefined '$uninitialized))
     (generate-immediate-ref t "undefined")
     #t)
    (('$closure-ref i)
     (generate-slot-ref t self-register (cells (+ i 2)))
     #t)
    (('quote c)
     (cond ((fixnum? c)
	    (generate-immediate-ref t (encode-fixnum c) "'" c))
	   ((eq? #t c)
	    (generate-true-ref t))
	   ((eq? #f c)
	    (generate-move t false-register))
	   ((null? c)
	    (generate-immediate-ref t "null"))
	   (else
	    (let ((l1 (register-literal c)))
	      (generate-immediate-ref t l1))))
     #t)
    ((op args ...)
     (translate-call x)
     #f)
    (_ (error "bad expression" x))))


;;; order argument-evulation to minimize spills
;
; - compute registers used for each argument.
; - identify circular dependencies between target registers and target-registers
;   of dependant arguments, and spill these cases to stack.
; - finally, topologically sort arguments by dependencies and evaluate in reverse
;   order.

(define (translate/registers args regs)
  (let* ((argc (length args))
	 (rargs (map (lambda (arg reg)
		       (list reg arg (delete-duplicates (used-registers arg))))
		     args
		     (append (take argc regs)
			     (iota (- argc (length regs)))))))
    (define (circular? ra rargs)
      (match-let (((tr _ deps) ra))
	(any (match-lambda
	       ((tr2 _ deps2) 
		(and (memv tr2 deps)
		     (memv tr deps2))))
	     rargs)))
    (define (translate-arguments spilled unspilled)
      (let* ((n (length spilled))
	     (reserve (cells n)))
	(pp `(SPILLED: ,spilled))	;XXX
	(pp `(UNSPILLED: ,unspilled))	;XXX
	(unless (zero? n)
	  (generate-reserve-on-stack reserve)
	  (do ((rargs spilled (cdr rargs))
	       (i 0 (add1 i)))
	      ((null? rargs))
	    (match-let ((((tr arg _) . _) rargs))
	      (let ((reg (argument-register arg)))
		(cond (reg (generate-slot-store stack-register (cells i) reg))
		      (else
		       (translate arg arg-register)
		       (generate-slot-store stack-register (cells i) arg-register)))))))
	(let ((sorted (reverse (topological-sort
				(map (match-lambda
				       ((tr _ deps) (cons tr deps)))
				     unspilled)
				eqv?))))
	  (pp sorted)			;XXX
	  (for-each
	   (lambda (sr)
	     (cond ((assv sr unspilled) =>
		    (match-lambda
		      ((tr arg _)
		       (cond ((symbol? tr) (translate arg tr))
			     (else 
			      (translate arg arg-register)
			      (generate-move-to-local (cells tr) arg-register))))))))
	   sorted))
	(unless (zero? n)
	  (do ((rargs spilled (cdr rargs))
	       (i 0 (add1 i)))
	      ((null? rargs))
	    (match-let ((((tr arg _) . _) rargs))
	      (cond ((symbol? tr)
		     (generate-slot-ref tr stack-register (cells i)))
		    (else
		     (generate-slot-ref arg-register stack-register (cells i))
		     (generate-move-to-local (cells tr) arg-register)))))
	  (generate-pop-stack reserve))))
    (pp rargs)				;XXX
    (let loop ((rargs rargs) (spilled '()) (unspilled '()))
      (match rargs
	(()
	 (translate-arguments spilled unspilled))
	((ra . more)
	 (if (circular? ra (append unspilled rargs))
	     (loop more (cons ra spilled) unspilled)
	     (loop more spilled (cons ra unspilled))))))))


;;; compute set of registers used by an expression
;
; - does not remove duplicates.
; - does not take the target-register into account.

(define (used-registers x)
  (match x
    (('$closure id cap . _)
     (cons arg-register (append-map used-registers cap)))
    (('let ((vars vals) ...) body)
     (fluid-let ((environment environment)
		 (locals-counter locals-counter)
		 (available-registers available-registers))
       (let ((newenv environment))
	 (append
	  (append-map
	   (lambda (var val)
	     (cond ((eq? var '$unused)
		    (if (simple-expression? val)
			'()
			(cons arg-register (used-registers val))))
		   ((null? available-registers)
		    (push! (cons var locals-counter) newenv)
		    (inc! locals-counter)
		    (cons arg-register (used-registers val)))
		   (else
		    (let ((reg (car available-registers)))
		      (push! (cons var reg) newenv)
		      (pop! available-registers)
		      (cons* arg-register reg (used-registers val))))))
	   vars vals)
	  (begin 
	    (set! environment newenv)
	    (used-registers body))))))
    (('$global-set! var val) (used-registers val))
    (('$global-ref var) '())
    (('$local-ref var)
     (let ((ref (lookup-variable var)))
       (if (symbol? ref)
	   (list ref)
	   '())))
    (('$local-set! var val)
     (let ((ref (lookup-variable var)))
       (append
	(if (symbol? ref)
	    (list ref)
	    '())
	(used-registers val))))
    (('if x y z)
     (append
      (used-registers x)
      (used-registers y)
      (used-registers z)))
    (('$primitive (or ('quote name) name)) '())
    (('$box val) (used-registers val))
    (('$box-ref val) (used-registers val))
    (('$box-set! box val)
     (append
      temporary-registers
      (used-registers box)
      (used-registers val)))
    (('$inline (or ('quote opr) opr) args ...)
     (append 
      (take (length args) temporary-registers)
      (append-map used-registers args)))
    (('$allocate (or ('quote type) type) (or ('quote size) size) args ...)
     (append
      (take (length args) temporary-registers)
      (append-map used-registers args)))
    (((or '$undefined '$uninitialized)) '())
    (('$closure-ref i) (list self-register))
    (('quote c) '())
    ((op args ...)
     (error "CPS-call in non-tail position" x))
    (_ (error "bad expression" x))))


;; return register that holds this value of #f
(define (argument-register arg)
  (match arg
    (('$local-ref var)
     (let ((reg (lookup-variable var)))
       (and (symbol? reg) reg)))
    (_ #f)))

(define (translate-call x)
  (let ((n (length x)))
    (translate/registers x argument-registers)
    (generate-slot-ref arg-register self-register (cells 1))
    (generate-immediate-ref count-register n)
    (if allocating
	(generate-alloc-check-and-call)
	(generate-tail-call arg-register))))

(define (translate-closure exp)
  (match exp
    (('$closure id cap (llists bodies) ...)
     (emit "\nf_" id ":\n")
     (do ((i 0 (add1 i))
	  (llists llists (cdr llists))
	  (bodies bodies (cdr bodies)))
	 ((null? llists))
       (let ((vars argc rest (parse-lambda-list (car llists)))
	     (next (string-append "f_c_" (number->string id) "_" (number->string (add1 i)))))
	 (unless (null? (cdr llists))
	   (generate-argc-check (add1 argc) rest next))
	 (translate-llist (car llists))
	 (set! allocating #f)
	 (translate (car bodies) arg-register)
	 (unless (null? (cdr llists))
	   (emit next ":\n")))))))

(define (translate-llist llist)
  (set! available-registers (cdr argument-registers))
  (set! locals-counter 0)
  (set! environment '())
  (let ((vars argc rest (parse-lambda-list llist))
	(unused '()))
    (for-each
     (lambda (var)
       (cond ((null? available-registers)
	      (unless (eq? var '$unused)
		(push! (cons var locals-counter) environment)
		(inc! locals-counter)))
	     ((eq? var '$unused)
	      ;; this register is not used for arguments but may be later used in "let" bindings
	      (push! (pop! available-registers) unused))
	     (else
	      (let ((reg (pop! available-registers)))
		(push! (cons var reg) environment)))))
     vars)
    ;; add unused argument registers back to available registers
    (set! available-registers (append unused available-registers))
    (generate-comment environment)
    (when (and rest (not (eq? '$unused rest)))
      (let ((rdest (cdr (assq rest environment))))
	(generate-immediate-ref arg-register (add1 argc))
	(generate-call "consrest")
	(if (symbol? rdest)
	    (generate-move rdest arg-register)
	    (generate-move-to-local (cells rdest) arg-register))))))

(define (translate-literal l c)
  (cond ((fixnum? c) (generate-equ l "(" c " << 1) | 1"))
	((number? c)
	 (cond ((= word-size 4)
		(generate-align 4)
		(generate-padding 4))
	       (else (generate-align word-size))) ; assumes word-size == 8
	 (emit l ": ")
	 (generate-defword "FLONUM | " (cells 1))
	 (generate-deffloat (exact->inexact c)))
	((pair? c)
	 (let ((lcar (register-literal (car c)))
	       (lcdr (register-literal (cdr c))))
	   (generate-align word-size)
	   (emit l ": ")
	   (for-each generate-defword (list "PAIR | 2" lcar lcdr))))
	((vector? c)
	 (let ((ls (map register-literal (vector->list c))))
	   (generate-align word-size)
	   (emit l ": ")
	   (generate-defword "VECTOR | " (length ls))
	   (for-each generate-defword ls)))
	((string? c)
	 (push! (cons l c) string-literals))
	((symbol? c)
	 (cond ((assq c symbol-table) =>
		(match-lambda 
		  ((_ . l2) (generate-equ l l2))))
	       (else
		(let ((l1 (register-literal (symbol->string c))))
		  (push! (cons c l) symbol-table)
		  (generate-align word-size)
		  (emit l ": ")
		  (generate-defword "SYMBOL | 1")
		  (generate-defword l1)))))
	((char? c)
	 (generate-align word-size)
	 (emit l ": ")
	 (generate-defword "CHAR | 1")
	 (generate-defword "(" (char->integer c) " << 1) | 1"))
	((null? c)
	 (generate-equ l "null"))
	((eq? c #t)
	 (generate-equ l "true"))
	((eq? c #f)
	 (generate-equ l "false"))
	(else (error "bad literal" c))))

(define (encode-fixnum n)
  (bitwise-ior (arithmetic-shift n 1) 1))

(define (lookup-variable var)
  (cond ((assq var environment) =>
	 (match-lambda 
	   ((_ . r)
	    (if (symbol? r)
		r
		(cells r)))))
	(else (error "unknown local variable" var))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (generate-globals defs)
  (generate-section ".data")
  (emit "globals:\n")
  (for-each 
   (lambda (def)
     (emit (mangle-identifier def) ": ")
     (generate-defword "undefined"))
   defs)
  (emit "endglobals:\n"))

(define (generate-closures top)
  (set! closures-to-be-translated (list top))
  (generate-section ".text")
  (emit "toplevel:\n")
  (do ()
      ((null? closures-to-be-translated))
    (translate-closure (pop! closures-to-be-translated))))

(define (generate-literals)
  (set! string-literals '())
  (set! symbol-table '())
  (generate-section ".data")
  (do ()
      ((null? literals-to-be-translated))
    (match-let (((l . c) (pop! literals-to-be-translated)))
      (translate-literal l c)))
  (generate-strings)
  (generate-symbol-table))

(define (generate-strings)
  (generate-section ".data")
  (for-each
   (match-lambda
     ((l . str)
      (generate-align word-size)
      (emit l ": ")
      (generate-defword "STRING | " (string-length str))
      (when (positive? (string-length str))
	(generate-defbyte 
	 (join (map (o number->string char->integer) (string->list str)) ",")))))
   string-literals))

(define (generate-symbol-table)
  (generate-section ".data")
  (emit "symbol_literals:\n")
  (for-each
   (lambda (l)
     (generate-defword (cdr l)))
   symbol-table)
  (generate-defword "false"))
