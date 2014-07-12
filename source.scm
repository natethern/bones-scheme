;;;; operations on source expressions - BONES-specific version of the one found in grass-lib


(define (parse-lambda-list llist)	; -> vars argc rest
  (let loop ((ll llist) (vars '()))
    (cond ((null? ll) 
	   (values (reverse vars) (length vars) #f))
	  ((symbol? ll) 
	   (values (reverse (cons ll vars)) (length vars) ll))
	  ((pair? ll) 
	   (loop (cdr ll) (cons (car ll) vars)))
	  (else (error "invalid lambda-list" llist)))))

(define (build-lambda-list vars argc rest)
  (append (take argc vars) (or rest '())))


(define (fragment exp . depth)
  (let ((depth (optional depth 3)))
    (define (walk x d)
      (if (> d depth)
	  '...
	  (cond ((vector? x) (list->vector (walk (vector->list x) d)))
		((pair? x)
		 (let loop ((x x) (n depth))
		   (cond ((null? x) '())
			 ((zero? n) '(...))
			 ((pair? x) (cons (walk (car x) (add1 d)) (loop (cdr x) (sub1 n))))
			 (else x))))
		(else x))))
    (walk exp 1)))


;; note: also returns #f for '$call
(define (procedure-call-expression? exp)
  (and (pair? exp) 
       (not (memq (car exp)
		  '($inline $allocate if begin $primitive quote letrec* let define set!
			    $case-lambda $lambda $undefined $uninitialized)))))

;; test if expression does not need any registers, mostly those
;; that just need a single machine-instruction
(define (simple-expression? exp)
  (match exp
    ;;XXX $allocate?
    ((or (? symbol?)
	 ('quote _)
	 '($undefined)
	 '($uninitialized)
	 ('$closure-ref _)
	 ('$box-ref (? simple-expression?))
	 ('$global-ref _)
	 ('$local-ref _))
     #t)
    (_ #f)))

;; test if expression is side-effect free
(define (pure-expression? exp)
  (match exp
    ((or (? symbol?)
	 ('quote _)
	 ('$closure _ ((? pure-expression?) ...) . _)
	 ('$allocate _ _ (? pure-expression?) ...)
	 '($undefined)
	 '($uninitialized)
	 ('$closure-ref _)
	 ('$box-ref (? pure-expression?))
	 ('$global-ref _)
	 ('$local-ref _))
     #t)
    (_ #f)))


;; Convert to canonical form
;
; - convert 2-arg if to 3-arg form.
; - quote all literals.
; - expand calls to "manifest" lambdas into "let" bindings.
; - "begin" forms only have 2 subforms.
; - does a few simplifications (empty bindings, begin-flattening, etc.)
; - marks "lambda" forms with id (converting them to "$lambda").
; - simplifies "$case-lambda".
; - also marks user lambdas as 'user entries in LDB.
; - does alpha-conversion.

(define lambda-id-counter 0)

(define (canonicalize-expression form)	; expects expanded form
  (define (resolve var env)
    (cond ((assq var env) => cdr)
	  (else var)))
  (let walk ((x form) (env '()))
    (match x
      ((or (? boolean?) (? number?) (? char?) (? string?) (? vector?)) `',x)
      ((? symbol?) (resolve x env))
      (('quote _) x)
      ((('lambda llist body ...) args ...)
       (let loop ((vars llist) (args args) (bs '()))
	 (cond ((null? vars)
		(if (null? args)
		    (walk `(let ,(reverse bs) ,@body) env)
		    (error "too many arguments in manifest `lambda' call" x)))
	       ((symbol? vars)
		(walk `(let ,(append (reverse bs) `((,vars (%list ,@args)))) ,@body)
		      env))
	       ((null? args)
		(error "too few arguments in manifest `lambda' call" x))
	       ((pair? vars)
		(loop (cdr vars) (cdr args) (cons (list (car vars) (car args)) bs)))
	       (else (error "invalid lambda list" llist)))))
      ((('$case-lambda ('lambda llists . bodies) ...) args ...)
       (let loop1 ((llists llists) (bodies bodies))
	 (when (null? llists)
	   (error "no matching clause in manifest `case-lambda' call" x))
	 (let loop2 ((vars (car llists)) (args args) (bs '()))
	   (cond ((null? vars)
		  (if (null? args)
		      (walk `(let ,(reverse bs) ,@(car bodies)) env)
		      (loop1 (cdr llists) (cdr bodies))))
		 ((symbol? vars)
		  (walk `(let ,(append (reverse bs) `((,vars (%list ,@args)))) ,@(car bodies)) env))
		 ((null? args)
		  (loop1 (cdr llists) (cdr bodies)))
		 ((pair? vars)
		  (loop2 (cdr vars) (cdr args) (cons (list (car vars) (car args)) bs)))
		 (else (error "invalid lambda list" (car llists)))))))
      (((or 'letrec* 'let) () body ...)
       (walk `(begin ,@body) env))
      (('letrec* ((vars vals) ...) body ...)
       (let* ((rvars (map (lambda (var) (cons var (rename-var var))) vars))
	      (env (append rvars env)))
	 `(,(car x) ,(map (lambda (rvar val) (list (cdr rvar) (walk val env))) rvars vals)
	   ,(walk `(begin ,@body) (append rvars env)))))
      (('let ((vars vals) ...) body ...)
       (let ((rvars (map (lambda (var) (cons var (rename-var var))) vars)))
	 `(,(car x) ,(map (lambda (rvar val) (list (cdr rvar) (walk val env))) rvars vals)
	   ,(walk `(begin ,@body) (append rvars env)))))
      (('begin ('begin xs1 ...) more ...)
       (walk `(begin ,@xs1 ,@more) env))
      (('$primitive name) x)
      (('$inline name xs ...)
       `($inline ,name ,@(map (cut walk <> env) xs)))
      (('$allocate t s xs ...)
       `($allocate ,t ,s ,@(map (cut walk <> env) xs)))
      (('begin x) (walk x env))
      (('begin x1 xs ...)
       `(begin ,(walk x1 env) ,(walk `(begin ,@xs) env)))
      (('lambda llist body ...)
       (let* ((id (inc! lambda-id-counter))
	      (vars argc rest (parse-lambda-list llist))
	      (rvars (map (lambda (var) (cons var (rename-var var))) vars)))
	 `($lambda ,id ,(build-lambda-list (map cdr rvars) argc (and rest (cdr (last rvars))))
		   ,(walk `(begin ,@body) (append rvars env)))))
      (('$case-lambda ('lambda llist body ...))
       (walk `(lambda ,llist ,@body) env))
      (('$case-lambda ('lambda llists . bodies) ...)
       (let ((id (inc! lambda-id-counter)))
	 `($case-lambda 
	   ,id
	   ,@(map (lambda (llist body)
		    (let* ((vars argc rest (parse-lambda-list llist))
			   (rvars (map (lambda (var) (cons var (rename-var var))) vars)))
		      (list (build-lambda-list (map cdr rvars) argc (and rest (cdr (last rvars))))
			    (walk `(begin ,@body) (append rvars env)))))
		  llists bodies))))
      (('if x y) (walk `(if ,x ,y ($undefined)) env))
      (('if x y z) `(if ,(walk x env) ,(walk y env) ,(walk z env)))
      (('set! var x)
       (let ((x (walk x env)))
	 `(set! ,(resolve var env) ,x)))
      (('define v x) `(define ,v ,(walk x env)))
      ((op args ...) (map (cut walk <> env) x))
      (_ (error "invalid expression" x)))))


;;; detect unused local variables
;
; - also removes "let" bindings for unused variables bound to a "pure" (side-efect free) value.
; - removes empty "let" expressions.

(define (detect-unused-variables form) ; expects expanded + canonicalized form
  (let ((globals '()))
    (define (used var env where)
      (cond ((assq var env) => (cut set-cdr! <> #t))
	    ((assq var globals) =>
	     (lambda (a)
	       (set-cdr! a (adjoin (cdr a) where))))
	    (else (push! (list var where) globals)))
      var)
    (define (defglobal var)
      (cond ((assq var globals))
	    (else (push! (list var) globals))))
    (define (used? var env)
      (cond ((assq var env) => cdr)
	    (else #f)))
    (define (destination var env)
      (and (not (assq var env)) var))
    (define (walk x env here dest)
      (match x
	((? symbol?) (used x env here))
	(('quote _) x)
	(((or 'letrec* 'let) ((vars vals) ...) body)
	 (let* ((env2 (append (map (cut cons <> #f) vars) env))
		(body (walk body env2 here dest))
		(eenv (if (eq? 'letrec* (car x)) env2 env))
		(vals (map (cut walk <> eenv here <>) vals vars)))
	   (let loop ((vars vars) (vals vals) (new '()))
	     (cond ((null? vars)
		    (if (null? new)
			body		; drop "let" entirely
			(list (car x) (reverse new) body)))
		   ((used? (car vars) env2)
		    (loop (cdr vars) (cdr vals) (cons (list (car vars) (car vals)) new)))
		   ((pure-expression? (car vals))
		    (loop (cdr vars) (cdr vals) new)) ; drop binding
		   (else (loop (cdr vars) (cdr vals) (cons (list '$unused (car vals)) new)))))))
	(('begin x) (walk x env here dest))
	(('begin x1 xs ...)
	 `(begin ,(walk x1 env here #f) ,(walk `(begin ,@xs) env here dest)))
	(('$lambda id llist body)
	 (let* ((vars argc rest (parse-lambda-list llist))
		(env2 (append (map (cut cons <> #f) vars) env))
		(body (walk body env2 dest #f)))
	   `($lambda ,id ,(build-lambda-list 
			   (map (lambda (var) (if (used? var env2) var '$unused)) vars)
			   argc
			   (and rest (if (used? rest env2) rest '$unused)))
		     ,body)))
	(('$case-lambda id (llists bodies) ...)
	 `($case-lambda
	   ,id
	   ,@(map (lambda (llist body)
		    (let* ((vars argc rest (parse-lambda-list llist))
			   (env2 (append (map (cut cons <> #f) vars) env))
			   (body (walk body env2 dest #f)))
		      (list (build-lambda-list 
			     (map (lambda (var) 
				    (if (used? var env2) var '$unused)) vars)
			     argc
			     (and rest (if (used? rest env2) rest '$unused)))
			    body)))
		  llists bodies)))
	(('if x y z)
	 `(if ,(walk x env here #f) ,(walk y env here dest) ,(walk z env here dest)))
	(('set! var x)
	 (if (assq var env)
	     (used var env here)
	     (defglobal var))
	 (let ((x (walk x env here (destination var env))))
	   `(set! ,var ,x)))
	(('define var x)
	 (defglobal var)
	 `(define ,var ,(walk x env here (destination var env))))
	(('$primitive n) x)
	(('$inline n xs ...)
	 `($inline ,n ,@(map (cut walk <> env here #f) xs)))
	(('$allocate t s xs ...)
	 `($allocate ,t ,s ,@(map (cut walk <> env here #f) xs)))
	((op args ...) (map (cut walk <> env here #f) x))
	(_ (error "invalid expression" x))))
    (let ((form (walk form '() #f #f)))
      ;; now remove unused global variables iteratively
      (let loop ((globals globals) (unused '()))
	(let ((ulist 
	       (filter-map
		(lambda (global)
		  (and (null? (cdr global)) (car global)))
		globals)))
	  (if (null? ulist)
	      (values form unused)
	      (loop
	       (filter-map
		(lambda (global)
		  (and (not (memq (car global) ulist))
		       (cons (car global)
			     (difference (cdr global) ulist))))
		globals)
	       (append ulist unused))))))))


;; variable renaming
(define rename-counter 1)
(define renamed-variables '())		; ((ALIAS1 . ORIG1) ...)

(define (rename-var v)
  (let ((var (string->symbol (string-append (symbol->string v) "^" (number->string rename-counter)))))
    (inc! rename-counter)
    (push! (cons var v) renamed-variables)
    var))

(define (genvar) (genvar/prefix ""))

(define (genvar/prefix prefix)
  (let ((var (string->symbol (string-append prefix "^" (number->string rename-counter)))))
    (inc! rename-counter)
    var))


;;; dump expressions, optionally in "lambda" format

(define (dump-expressions form ldump . port)
  (let ((port (optional port (current-output-port))))
    (if (not ldump)
	(pp form port)
	(let ((ls (list form)))
	  (define (prepare x)
	    (match x
	      ((or (? boolean?) (? number?) (? char?) (? string?) ('quote _) (? symbol?))
	       x)
	      (('$lambda id . _)
	       (push! x ls)
	       `($lambda ,id ...))
	      (('$case-lambda id . _)
	       (push! x ls)
	       `($case-lambda ,id ...))
	      (('$closure id . _)
	       (push! x ls)
	       `($closure ,id ...))
	      (((and op (or 'let 'letrec*)) ((vars vals) ...) xs ...)
	       (cons* op (map (lambda (var val) (list var (prepare val))) vars vals)
		      (map prepare xs)))
	      (_ (map prepare x))))
	  (do () ((null? ls))
	    (let ((ls1 (reverse ls)))
	      (set! ls '())
	      (for-each
	       (match-lambda
		(('$lambda id llist xs ...)
		 (pp `($lambda ,id ,llist ,@(map prepare xs)) port))
		(('$case-lambda id (llists . bodies) ...)
		 (pp `($case-lambda ,id ,@(map (lambda (ll xs) (cons ll (map prepare xs))) llists bodies)) port))
		(('$closure id cap (llists bodies) ...)
		 (pp `($closure ,id ,cap 
				,@(map (lambda (ll bd) (list ll (prepare bd))) llists bodies))
		     port))
		(form
		 (pp (prepare form) port)))
	       ls1)))))))
