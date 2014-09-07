;;;; unused variable elimination (UVE)


;;; detect unused local variables
;
; - also removes "let" bindings for unused variables bound to a "pure" (side-efect free) value.
; - removes empty "let" expressions.
; - special cases "%error" and "%interrupt-hook", as these is invoke from boneslib.s


(define implicitly-retained-globals '(%error %interrupt-hook))


(define (detect-unused-variables form) ; expects expanded + canonicalized form
  (let ((globals '())
	(dcount 0))
    (define (used var env where)
      (cond ((eq? var where))
	    ((assq var env) => (cut set-cdr! <> #t))
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
		    (inc! dcount)
		    (loop (cdr vars) (cdr vals) new)) ; drop binding
		   (else (loop (cdr vars) (cdr vals) (cons (list '$unused (car vals)) new)))))))
	(('begin x) (walk x env here dest))
	(('begin x1 xs ...)
	 `(begin ,(walk x1 env here #f) ,(walk `(begin ,@xs) env here dest)))
	(('$lambda id llist body)
	 (let* ((vars argc rest (parse-lambda-list llist))
		(env2 (append (map (cut cons <> #f) vars) env))
		(body (walk body env2 (or here dest) #f)))
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
			   (body (walk body env2 (or here dest) #f)))
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
	(('$inline-test n cnd xs ...)
	 `($inline-test ,n ,cnd ,@(map (cut walk <> env here #f) xs)))
	(('$allocate t s xs ...)
	 `($allocate ,t ,s ,@(map (cut walk <> env here #f) xs)))
	(('$call id xs ...)
	 `($call ,id ,@(map (cut walk <> env here #f) xs)))
	((op args ...) (map (cut walk <> env here #f) x))
	(_ (error "invalid expression" x))))
    (used '%error '() #f)
    (let ((form (walk form '() #f #f))
	  (ucount 0))
      ;; now remove unused global variables iteratively
      (let loop ((globals globals) (unused '()))
	(let ((ulist 
	       (filter-map
		(lambda (global)
		  (and (null? (cdr global)) (car global)))
		globals)))
	  (inc! ucount (length ulist))
	  (cond ((null? ulist)
		 (NB "  removed " ucount " global variables")
		 (NB "  dropped " dcount " pure local expressions")
		 (values form unused))
		(else
		 (loop
		  (filter-map
		   (lambda (global)
		     (and (not (memq (car global) ulist))
			  (cons (car global)
				(difference (cdr global) ulist))))
		   globals)
		  (append ulist unused)))))))))
