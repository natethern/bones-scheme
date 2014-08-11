;;;; register assignment


(define available-registers '())

(define (blocked-register? reg)
  (not (memq reg available-registers)))


;; Return source register or #f, depending on whether the expression already
;; resides in a register
(define (trivial-register-expression? exp)
  (match exp
    (('$local-ref var)
     (let ((ref (lookup-variable var)))
       (and (symbol? ref) ref)))
    (('quote #f) 'FALSE)
    (_ #f)))


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


;;; order argument-evaluation to minimize spills
;
; - compute registers and locals used for each argument.
; - identify circular dependencies between target registers and target-registers
;   of dependant arguments, and spill these cases to stack.
; - finally, topologically sort arguments by dependencies and evaluate in reverse
;   order.
; - returns list of argument registers / locals associated with given arguments.

(define (translate/registers args regs)
  (let* ((argc (length args))
	 (rargs (map (lambda (arg reg)
		       (list reg arg (delete-duplicates (used-registers arg))))
		     args
		     (append (take argc regs)
			     (iota (- argc (length regs)))))))
    (define (circular? ra rargs)
      ;;XXX special case: arg depends on own target register - could be
      ;;    ignored, but will make tsort fail
      (define (follow r done)
	(or (memq r done)
	    (let ((done (cons r done)))
	      (cond ((assq r rargs) =>
		     (match-lambda 
		       ((r2 _ deps)
			(any (cut follow <> done) deps))))
		    (else #f)))))
      (any (cut follow <> '()) (caddr ra)))
    (define (translate-arguments spilled unspilled)
      (let* ((n (length spilled))
	     (reserve (cells n)))
	;;(pp `(SPILLED: ,spilled))	;XXX
	;;(pp `(UNSPILLED: ,unspilled))	;XXX
	(unless (zero? n)
	  (generate-reserve-on-stack reserve)
	  (do ((rargs spilled (cdr rargs))
	       (i 0 (add1 i)))
	      ((null? rargs))
	    (match-let ((((_ arg _) . _) rargs))
	      (let ((reg (argument-register arg)))
		(cond ((symbol? reg)
		       (generate-slot-store stack-register (cells i) reg))
		      (else
		       (translate arg arg-register)
		       (generate-slot-store stack-register (cells i) arg-register)))))))
	(let* ((dag (map (match-lambda
			   ((tr _ deps) (cons tr deps)))
			 unspilled))
	       (sorted (topological-sort dag eqv?)))
	  ;;(pp `(DAG: ,@dag))			;XXX
	  ;;(pp `(SORTED: ,@sorted))			;XXX
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
    ;;(pp `(RARGS: ,@rargs))				;XXX
    (let loop ((ras rargs) (spilled '()) (unspilled '()))
      (match ras
	(()
	 (translate-arguments spilled unspilled)
	 (map car rargs))
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
	  (concatenate
	   (map
	    (lambda (var val)
	      (cond ((eq? var '$unused)
		     (if (simple-expression? val)
			 '()
			 (cons arg-register (used-registers val))))
		    ((null? available-registers)
		     (push! (cons var locals-counter) newenv)
		     (inc! locals-counter)
		     (cons* 
		      arg-register
		      (sub1 locals-counter)
		      (used-registers val)))
		    (else
		     (let ((reg (car available-registers)))
		       (push! (cons var reg) newenv)
		       (pop! available-registers)
		       (cons* arg-register reg (used-registers val))))))
	    vars vals))
	  (begin 
	    (set! environment newenv)
	    (used-registers body))))))
    (('$global-set! var val) (used-registers val))
    (('$global-ref var) '())
    (('$local-ref var) (list (lookup-variable var)))
    (('$local-set! var val)
     (let ((ref (lookup-variable var)))
       (append
	(list ref)
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
    (('$inline _ args ...)
     (append 
      temporary-registers      ; inline code may clobber any temporary
      (append-map used-registers args)))
    (('$inline-test _ _ args ...)
     (append
      temporary-registers
      (append-map used-registers args)))
    (('$allocate _ _ args ...)
     (append
      (take (length args) temporary-registers)
      (append-map used-registers args)))
    (((or '$undefined '$uninitialized)) '())
    (('$closure-ref _) (list self-register))
    (('quote _) '())
    (('$call _ ...)
     (error "CPS-call in non-tail position" x))
    ((op _ ...)
     (error "CPS-call in non-tail position" x))
    (_ (error "RA: invalid expression" x))))


;; return register that holds this value of #f
(define (argument-register arg)
  (match arg
    (('$local-ref var) (lookup-variable var))
    (_ #f)))

