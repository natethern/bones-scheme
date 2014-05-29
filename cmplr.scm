;;;; bare bones scheme compiler


(define basic-implementation-features '(bones srfi-0 srfi-6 srfi-8 srfi-7))

(define argument-register-count 0)     ; set later
(define primitives '())
(define closures-to-be-translated '())
(define literals-to-be-translated '())
(define string-literals '())
(define symbol-table '())
(define label-counter 0)
(define environment '())
(define allocating #f)

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
	       (_ code))))
    (when (option 'dump-features: options)
      (for-each print implementation-features)
      (exit))
    (expand-syntax (generate-cond-expand implementation-features))
    (let* ((code (expand-syntax prg))
	   (_ (when (option 'expand: options) 
		(pp code)
		(exit)))
	   (dumpcc (option 'dump-cc: options))
	   (dumpcps (option 'dump-cps: options))
	   (dumpserial (not (option 'dump-nested: options)))
	   (dumpcompiled (option 'dump: options))
	   (outfile (option 'output-file: options))
	   (code (canonicalize-expression code))
	   (defs code (cps code))
	   (_ (when dumpcps
		(dump-expressions code dumpserial)
		(exit)))
	   (code (detect-unused-variables code))
	   (_ (when dumpcompiled
		(dump-expressions code dumpserial)
		(exit)))
	   (ccode (cc code '())))
      (when dumpcc
	(dump-expressions ccode dumpserial)
	(exit))
      ;;XXX add pass that assigns closure-id's to target variables, for adding comments in
      ;;    generated output.
      ((if outfile
	   (lambda (thunk)
	     (with-output-to-file outfile thunk))
	   (lambda (thunk) (thunk)))
       (cut generate-code defs ccode)))))

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
    ;;XXX $inline? $allocate?
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
  (translate/registers args temporary-registers #f))

(define (blocked-register? reg)
  (or (eq? reg self-register)
      (any (match-lambda ((_ . r) (eq? r reg))) environment)))

(define (translate x t)
  ;;(pp (if (pair? x) (car x) x))
  (match x
    (('$closure id cap _ _)
     (push! x closures-to-be-translated)
     (set! allocating #t)
     (generate-closure-alloc t (length cap) id)
     (do ((lst cap (cdr lst))
	  (off 2 (add1 off)))
	 ((null? lst))
       (translate (car lst) arg-register)
       (generate-slot-store alloc-register (cells off) arg-register))
     (generate-move t alloc-register)
     (generate-add alloc-register (cells (+ 2 (length cap))))
     #t)
    (('let ((vars vals) ...) body)
     (let* ((oldenv environment)
	    (env (extend-environment environment vars))
	    (single (= 1 (length vars))))
       ;; evaluate vals and push on stack
       (unless single
	 (generate-reserve-on-stack (cells (length vars))))
       (do ((vars vars (cdr vars))
	    (vals vals (cdr vals))
	    (i 0 (add1 i)))
	   ((null? vars))
	 (let ((var (car vars))
	       (val (car vals)))
	   (cond ((assq var env) => 
		  (lambda (a)
		    (cond (single
			   (let ((dest (cdr a)))
			     (cond ((symbol? dest)
				    (translate val arg-register)
				    (generate-move dest arg-register))
				   (else
				    (translate val arg-register)
				    (generate-move-to-local (cells dest) arg-register)))))
			  (else
			   (translate val arg-register)
			   (generate-slot-store stack-register (cells i) arg-register)))))
		 ((simple-expression? val))   ; unused and simple
		 (else (translate val arg-register))))) ; unused but not simple
       (unless single
	 (do ((vars vars (cdr vars))
	      (vals vals (cdr vals))
	      (i 0 (add1 i)))
	     ((null? vars))
	   (let ((var (car vars))
		 (val (cdr vals)))
	     (cond ((assq var env) =>
		    (lambda (a)
		      (let ((dest (cdr a)))
			(generate-comment dest " = " var)
		       (cond ((symbol? dest)
			      (generate-slot-ref dest stack-register (cells i)))
			     (else
			      (generate-slot-ref arg-register stack-register (cells i))
			      (generate-move-to-local (cells dest) arg-register)))))))))
	 (generate-pop-stack (cells (length vars))))
       (fluid-let ((environment env))
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
       (unless (eq? t r2) (generate-move t r2))
       #t))
    (('$inline (or ('quote opr) opr) args ...)
     (assert (<= (length args) (length temporary-registers)) 
	     "too many arguments to `$inline'" args)
     (translate-inline-arguments args)
     (for-each
      (cut emit " " <> "\n")
      (string-split opr ";"))
     (unless (eq? t arg-register) (generate-move t arg-register))
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

;;xxx replace this with a decent ra
(define (translate/registers args regs locals?)
  (let* ((argc (length args))
	 (rargs (map cons args 
		     (append (take argc regs)
			     (iota (- argc (length regs)))))))
    (match rargs
      (((arg . reg))			; just a single argument
       (translate arg arg-register)
       (if (symbol? reg)
	   (unless (eq? reg arg-register) (generate-move reg arg-register))
	   (generate-move-to-local (cells reg) arg-register)))
      (_ (let* ((easy hard (partition
			    (match-lambda
			      ((arg . reg)
			       (and (simple-expression? arg)
				    ;;xxx could check whether arg is a var already stored in reg
				    (not (blocked-register? reg))
				    (match arg
				      (('$closure-ref i) #f)
				      (('$local-ref var)
				       (not (memq (cdr (assq var environment)) regs)))
				      (_ #t)))))
			    rargs))
		(reserve (cells (length hard))))
	   (unless (null? hard)
	     (generate-reserve-on-stack reserve)
	     (do ((i 0 (add1 i))
		  (hard hard (cdr hard)))
		 ((null? hard))
	       (translate (caar hard) arg-register)
	       (generate-slot-store stack-register (cells i) arg-register)))
	   ;; assign easy destinations first to avoid clobbering rbx
	   (for-each
	    (match-lambda
	      ((arg . reg)
	       (cond ((symbol? reg) (translate arg reg))
		     (else
		      (translate arg arg-register)
		      (generate-move-to-local (cells reg) arg-register)))))
	    easy)
	   (unless (null? hard)
	     (do ((i 0 (add1 i))
		  (hard hard (cdr hard)))
		 ((null? hard))
	       (match (car hard)
		 ((_ . reg)
		  (cond ((symbol? reg)
			 (generate-slot-ref reg stack-register (cells i)))
			(else
			 (generate-slot-ref arg-register stack-register (cells i))
			 (generate-move-to-local (cells reg) arg-register))))))
	     (generate-pop-stack reserve)))))
    rargs))

(define (translate-call x)
  (let ((n (length x)))
    (translate/registers x argument-registers #t)
    (generate-slot-ref arg-register self-register (cells 1))
    (generate-immediate-ref count-register n)
    (if allocating
	(generate-alloc-check-and-call)
	(generate-tail-call arg-register))))

(define (translate-closure exp)
  (match exp
    ((_ id cap llist body)
     (emit "\nf_" id ":\n")
     (translate-llist llist)
     (set! allocating #f)
     (translate body arg-register))))

(define (translate-llist llist)
  (let* ((vars argc rest (parse-lambda-list llist))
	 (nvars (length vars))
	 (lvars (- nvars (length (cdr argument-registers)))))
    ;;XXX the registers for unused variables are not made available
    (set! environment
      (map cons
	   vars
	   (append 
	    (take nvars (cdr argument-registers))
	    (if (positive? lvars)
		(iota lvars)
		'()))))
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

(define (extend-environment env vars)
  (let* ((vars (filter (lambda (var) (not (eq? var '$unused))) vars))
	 (avail (max 0 (- argument-register-count (length env))))
	 (rcount (min argument-register-count (length env)))
	 (rvars lvars (split-at avail vars)))
    (append 
     (map cons rvars (take (length rvars) (drop rcount (cdr argument-registers))))
     (map cons lvars (iota (length lvars) (add1 (- (length env) (sub1 argument-register-count)))))
     env)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (command-line-option? str)
  (let ((len (string-length str)))
    (and (> len 1)
	 (char=? #\- (string-ref str 0)))))

(define (usage)
  (display "usage: bones [-o OUTFILE] [-L LIBRARY_PATH] [-feature FEATURE] [-dump] [-expand]" (current-error-port))
  (display "[-dump-cc] [-dump-nested] [-dump-cps] [-dump-features] FILENAME\n" (current-error-port))
  (exit 1))

(define (main args)
  (let ((opts '())
	(fname #f))
    (let loop ((args args))
      (match args
	(() 
	 (if fname
	     (apply compile-file fname opts)
	     (usage)))
	(("-o" out . more)
	 (set! opts (cons* 'output-file: out opts))
	 (loop more))
	(("-L" path . more)
	 (set! opts (append (append-map (cut list 'library-path: <>) (string-split path ":")) opts))
	 (loop more))
	(("-feature" f . more)
	 (set! opts (cons* 'feature: (string->symbol f) opts))
	 (loop more))
	(((? command-line-option? opt) . more)
	 (set! opts (cons* (string->symbol (string-append (substring opt 1 (string-length opt)) ":")) #t opts))
	 (loop more))
	((filename . more)
	 (set! fname filename)
	 (loop more))))))
