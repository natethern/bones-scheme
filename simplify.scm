;;;; simplification pass


;;; walk forms recursively and simplify
;
; - keeps environment: e = ((VAR USECOUNT ASSIGNED?) ...)

(define (simplify form)
  (let ((scount 0))
    (define (add-env vars e)
      (append (map (cut list <> #f #f) vars) e))
    (define (assigned! v)
      (set-car! (cddr v) #t))
    (define (used! v)
      (set-car! (cdr v) (add1 (or (cadr v) 0))))
    (define (use-count v e)
      (cond ((assq v e) => cadr)
	    (else #f)))
    (define (walk x e)
      ;;(pp (limited 3 x))
      (match x
	((? symbol?) 
	 (cond ((assq x e) => used!))
	 x)
	(('quote _) x)
	(('let ((vars vals) ...) body)
	 (let* ((vals (map (cut walk <> e) vals))
		(e2 (add-env vars e))
		(body (walk body e2)))

	   ;; a particular expansion of "not":
	   ;;
	   ;; (let ((VAR ($inline-test ...)))
	   ;;   (if VAR ...))                        ; VAR not used in "..."
	   (cond ((and (match vals
			 ((('$inline-test . _)) #t)
			 (_ #f))
		       (match body
			 (('if (? symbol? var) . _)
			  (and (eq? var (car vars))
			       (eqv? 1 (use-count var e2))))
			 (_ #f)))
		  (inc! scount)
		  `(if ,(car vals) ,@(cddr body)))

		 (else
		  `(let ,(map (lambda (var val) (list var val)) vars vals)
		     ,body)))))
	(('letrec* ((vars vals) ...) body)
	 (let ((e (add-env vars e)))
	   `(letrec* ,(map (lambda (var val) (list var (walk val e))) vars vals)
	      ,(walk body e))))
	(('begin x y)
	 `(begin ,(walk x e) ,(walk y e)))
	(('if x y z)
	 (let ((x (walk x e))
	       (y (walk y e))
	       (z (walk z e)))
	   (match x

	     ;; "not"
	     (('if x1 ''#f ''#t)
	      (inc! scount)
	      `(if ,x1 ,z ,y))

	     ;; just for completeness
	     (('if x1 ''#t ''#f)
	      (inc! scount)
	      `(if ,x1 ,y ,z))

	     (_ `(if ,x ,y ,z)))))
	(('$lambda id llist body)
	 (let ((vars argc rest (parse-lambda-list llist)))
	   `($lambda ,id ,llist ,(walk body (add-env vars e)))))
	(('$case-lambda id (llists bodies) ...)
	 `($case-lambda 
	   ,id 
	   ,@(map (lambda (llist body) 
		    (let ((vars argc rest (parse-lambda-list llist)))
		      (list llist (walk body (add-env vars e)))))
		  llists bodies)))
	(('set! var x)
	 (cond ((assq var e) => assigned!))
	 `(set! ,var ,(walk x e)))
	(('define var x)
	 `(define ,var ,(walk x e)))
	(('$primitive _) x)
	(('$inline name xs ...)
	 `($inline ,name ,@(map (cut walk <> e) xs)))
	(('$inline-test name cnd xs ...)
	 `($inline-test ,name ,cnd ,@(map (cut walk <> e) xs)))
	(('$allocate t s xs ...)
	 `($allocate ,t ,s ,@(map (cut walk <> e) xs)))
	(('$call id xs ...)
	 `($call ,id ,@(map (cut walk <> e) xs)))
	((xs ...)
	 (map (cut walk <> e) xs))
	(_ (error "SIMPLIFY: invalid expression" x))))
    (let ((form (walk form '())))
      (NB "  simplified " scount " forms")
      form)))
