;;;; bare bones scheme compiler


(define basic-implementation-features '(bones srfi-0 srfi-6 srfi-8 srfi-7))


(define (compile code . options)
  (set! lambda-id-counter 0)
  (set! implementation-features
    (append (map string->symbol (collect-options 'feature: options))
	    (list target-os target-arch)
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

(define primitives '())
(define closures-to-be-translated '())
(define literals-to-be-translated '())
(define string-literals '())
(define symbol-table '())
(define label-counter 0)
(define environment '())
(define allocating #f)

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
  (or (eq? reg 'rbx)
      (any (match-lambda ((_ . r) (eq? r reg))) environment)))

(define (encode-fixnum n)
  (bitwise-ior (arithmetic-shift n 1) 1))

(define (extend-environment env vars)
  (let* ((vars (filter (lambda (var) (not (eq? var '$unused))) vars))
	 (avail (max 0 (- argument-register-count (length env))))
	 (rcount (min argument-register-count (length env)))
	 (rvars lvars (split-at avail vars)))
    (append 
     (map cons rvars (take (length rvars) (drop rcount (cdr argument-registers))))
     (map cons lvars (iota (length lvars) (- (length env) (sub1 argument-register-count))))
     env)))

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
