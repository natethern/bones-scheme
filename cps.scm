;;;; CPS-conversion, taken and adapted from SPOCK, originally from EOPL
;
; - this pass performs a few simplifications:
;
;   * rewrites "letrec*" expressions into "let" + "set!"
;   * changes "define" into "set!"
;   * replaces "begin" forms with a chain of "let"-forms
;
; - returns list of defined toplevel variables and root expression


;; CPS-conversion algorithm from "Essentials of Programming Languages"
(define (cps form)     ; expects code to be expanded and canonicalized
  (let ((defs '())
	(counter 0))
    (define (temp . prefix)
      (inc! counter)
      (string->symbol (string-append (optional prefix "t") (number->string counter))))
    (define (k-id)
      (inc! lambda-id-counter))
    (define (zero x)
      (let ((k (temp "k")))
	`($lambda ,(k-id) (,k) ,(one x k))))	; Cpgm
    (define i 0)
    #;(define (one x k)
      (pp `(ONE: ,x ,k))
      (let ((y (one1 x k)))
	(pp `(ONE ,x ,k -> ,y))
	y))
    (define (one x k)
      ;;(pp `(ONE: ,x ,k))
      (match x
	(('let () body ...) (one `(begin ,@body) k))
	(('let ((v x) . more) body ...)
	 (if (simple? x)
	     `(let ((,v ,(two x)))	; Clet
		,(one `(let ,more ,@body) k))
	     (let ((t (temp)))		; Chead
	       (one x `($lambda ,(k-id) (,t)
				(let ((,v ,t))
				  ,(one `(let ,more ,@body) k)))))))
	(('letrec* ((vars vals) ...) body ...)
	 (one
	  `(let ,(map (lambda (var) (list var '($uninitialized))) vars)
	     ,@(map (lambda (var val) `(set! ,var ,val)) vars vals)
	     ,@body)
	  k))
	(('begin) (one '($undefined) k))
	(('begin x) (one x k))
	(('begin x1 body ...)
	 (if (simple? x1)
	     (one `(let ((,(temp) ,x1)) (begin ,@body)) k)
	     (let ((t (temp)))
	       (one x1 `($lambda ,(k-id) (,t)
				 ,(one `(begin ,@body) k))))))
	(('$inline _ (? simple?) ...)
	 (callk k (lambda () x)))
	(('$inline name xs ...)
	 (let ((temps (map (lambda _ (temp)) xs)))
	   (one `(let ,(map list temps xs)
		   ($inline ,name ,@temps))
		k)))
	(('$inline-test _ _ (? simple?) ...)
	 (callk k (lambda () x)))
	(('$inline-test name cnd xs ...)
	 (let ((temps (map (lambda _ (temp)) xs)))
	   (one `(let ,(map list temps xs)
		   ($inline-test ,name ,cnd ,@temps))
		k)))
	(('$allocate _ _ (? simple?) ...)
	 (callk k (lambda () x)))
	(('$allocate t s xs ...)
	 (let ((temps (map (lambda _ (temp)) xs)))
	   (one `(let ,(map list temps xs)
		   ($allocate ,t ,s ,@temps))
		k)))
	(('$primitive name)
	 (callk k (lambda () x)))
	((? simple?)
	 (callk k (lambda () x)))
	;; from here on `x' is non-simple
	(('set! v y)
	 (let ((t (temp)))
	   (one y `($lambda ,(k-id) (,t)		; Chead
			    (let ((,(temp) (set! ,v ,t))) ;XXX this produces an additional temporary,
			      ,(callk k (lambda () ''#f))))))) ; for some unknown reason
	(('if x y z)
	 (bindk
	  k
	  (lambda (k)
	    (if (simple? x)
		`(if ,(two x)		; Cif
		     ,(one y k)
		     ,(one z k))
		(let ((t (temp)))		; Chead
		  (one x `($lambda ,(k-id) (,t)
				   (if ,t 
				       ,(one y k)
				       ,(one z k)))))))))
	(('define var val)
	 (push! var defs)
	 (one `(set! ,var ,val) k))
	(((? simple?) ...)		; Capp
	 (cons (two (car x)) (cons k (map two (cdr x)))))
	((xs ...)
	 (head
	  xs
	  (lambda (xs2) (cons (car xs2) (cons k (cdr xs2))))))
	(else (error "one" x k))))
    (define (two x)
      ;;(pp `(TWO: ,x))			
      (match x
	((? symbol?) x)
	(('$lambda id llist body)	; Cproc
	 (let ((k (temp "k")))
	   `($lambda ,id (,k . ,llist) ,(one body k))))
	(('$case-lambda id (llists bodies) ...)	; Cproc
	 `($case-lambda
	   ,id
	   ,@(map (lambda (llist body)
		    (let ((k (temp "k")))
		      (list `(,k . ,llist) (one body k))))
		  llists bodies)))
	(('if xs ...) `(if ,@(map two xs)))
	(('$inline name xs ...) `($inline ,name ,@(map two xs)))
	(('$inline-test name cnd xs ...) `($inline-test ,name ,cnd ,@(map two xs)))
	(('$allocate t s xs ...) `($allocate ,t ,s ,@(map two xs)))
	(('set! v y) `(set! ,v ,(two y)))
	(((or 'quote '$undefined '$uninitialized '$primitive) . _) x)
	(('let ((var x)) y)
	 `(let ((,var ,(two x))) ,(two y)))
	((xs ...) (map two xs))
	(_ (error "two" x))))
    (define (bindk k proc)
      (if (symbol? k)
	  (proc k)
	  (let ((t (temp)))
	    `(let ((,t ,k))
	       ,(proc t)))))
    (define (callk k thunk)
      (let ((r (thunk)))
	(if (symbol? k)
	    `(,k ,(two r))		; Csimplevar
	    (let ((v (caaddr k)))	; Csimpleproc
	      `(let ((,v ,(two r)))
		 ,(cadddr k))))))
    (define (head xs wrap)
      (let loop ((xs xs) (xs2 '()))	; Chead
	(if (null? xs)
	    (wrap (reverse xs2))
	    (let ((x (car xs)))
	      (if (simple? x)
		  (loop (cdr xs) (cons (two x) xs2))
		  (let ((t (temp)))
		    (one x `($lambda ,(k-id) (,t) 
				     ,(loop (cdr xs) (cons t xs2))))))))))
    (define (simple? x)
      (match x
	(((or '$lambda '$case-lambda 'quote '$undefined '$uninitialized '$primitive) . _) #t)
	((? symbol?) #t)
	(('if (? simple?) ...) #t)
	(('let ((_ (? simple?)) ...) (? simple?) ...) #t)
	(('$inline _ (? simple?) ...) #t)
	(('$inline-test _ _ (? simple?) ...) #t)
	(('$allocate _ _ (? simple?) ...) #t)
	(('set! _ (? simple?)) #t)
	(_ #f)))
    (let ((result (zero form)))
      (values (delete-duplicates defs) result))))
