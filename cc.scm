;;;; closure-conversion algorithm
;
; - produces the following forms:
;
;   'C
;   (if X Y Z)
;   ($box X)
;   ($word-box X)
;   ($float-box X)
;   ($box-set! X Y)
;   ($word-box-set! X Y)
;   ($float-box-set! X Y)
;   ($box-ref X)
;   ($word-box-ref X)
;   ($float-box-ref X)
;   ($closure NAME (CAP1 ...) (LLIST1 X1) ...)
;   ($closure-ref I)
;   ($global-ref V)
;   ($global-set! V X)
;   ($local-ref V)
;   ($local-set! V X)
;   ($inline NAME X ...)
;   ($inline-test NAME CONDITION X ...)
;   ($allocate TYPE SIZE X ...)
;   ($primitive NAME)
;   (define V X)
;   (X1 X2 ...)
;   (let ((V X)) Y)
;   ($undefined)
;   ($uninitialized)
;   ($call ID X ...)
;
; - note that this pass introduces duplicate bindings for boxed variants and thus
;   breaks any alpha-conversion properties.
; - also doesn't properly separate assignment-elimination with closure-conversion
; - overall pretty ugly but working (and it took long enough to do so).
; - the passed "vtlist" will be modified for typed boxes, so that codegen will not try to
;   access the values directly and natively typed data (this is currently not used)


(define cc-box-counts '())

(define (cc form vtlist) ; expects form to be expanded, canonicalized and lambda-decorated
  (let ((fbcount 0)
	(wbcount 0)
	(bcount 0))
    (define (vartype var)
      (cond ((assq var vtlist) => cdr)
	    (else #f)))
    (define (unbox-op var)
      (case (vartype var)
	((fixnum) '$word-box-ref)
	((flonum) '$float-box-ref)
	(else '$box-ref)))
    (define (box-op var)
      (case (vartype var)
	((fixnum) '$word-box)
	((flonum) '$float-box)
	(else '$box)))
    (define (box-set-op var)
      (case (vartype var)
	((fixnum) '$word-box-set!)
	((flonum) '$float-box-set!)
	(else '$box-set!)))
    (define (fixup! e)
      (for-each
       (match-lambda
	 ((v assigned shared refs ...)
	  (let ((boxed (and assigned shared)))
	    (for-each
	     (match-lambda
	       ((and ref ('$closure-ref _))
		(when boxed
		  (set-car! ref (unbox-op v))
		  (set-car! (cdr ref) `($closure-ref ,v))))
	       ((and ref ('$local-ref _))
		(when boxed
		  (set-car! ref (unbox-op v))
		  (set-car! (cdr ref) `($local-ref ,v))))
	       ((and ref ('$local-set! _ x))
		(when boxed
		  (set-car! ref (box-set-op v))
		  (set-car! (cdr ref) `($local-ref ,v))))
	       (_ #f))
	     refs))))
       e))
    (define (mapwalk xs e)
      (let loop ((xs xs) (xs2 '()) (fv '()))
	(if (null? xs)
	    (values (reverse xs2) fv)
	    (let ((x2 fv2 (walk (car xs) e)))
	      (loop (cdr xs) (cons x2 xs2) (union fv fv2))))))
    (define (walk x e)
      ;; e = (((VAR1 ASSIGNED? SHARED? REF1 ...) ...) ...)
      ;;(pp `(WALK: ,x ,e))
      (match x
	((? symbol?)
	 (cond ((and (pair? e) (assq x (car e))) => ; local
		(lambda (a)
		  (let ((x2 `($local-ref ,x)))
		    (set-cdr! (cddr a) (cons x2 (cdddr a))) ; add to env-refs
		    (values x2 '()))))
	       ((and (pair? e) (any (cut assq x <>) (cdr e))) => ; lexical
		(lambda (a)
		  (let ((x2 `($closure-ref ,x)))
		    (set-car! (cddr a) #t) ; mark as shared
		    (set-cdr! (cddr a) (cons x2 (cdddr a))) ; add to env-refs
		    (values x2 (list x)))))
	       (else 
		(values `($global-ref ,x) '())))) ; global
	(('quote _) (values x '()))
	(((or '$uninitialized '$undefined)) (values x '()))
	(('$inline n xs ...)
	 (let ((xs2 fv (mapwalk xs e)))
	   (values `($inline ,n ,@xs2) fv)))
	(('$inline-test n cnd xs ...)
	 (let ((xs2 fv (mapwalk xs e)))
	   (values `($inline-test ,n ,cnd ,@xs2) fv)))
	(('$allocate t s xs ...)
	 (let ((xs2 fv (mapwalk xs e)))
	   (values `($allocate ,t ,s ,@xs2) fv)))
	(('$primitive name) (values x '()))
	(('if x1 xs ...)
	 (let ((x1 fv1 (walk x1 e))
	       (xs fv2 (mapwalk xs e)))
	   (values `(if ,x1 ,@xs) (union fv1 fv2))))
	(('let ((vars vals) ...) body)
	 (let* ((e0 (car e))
		(e2 (cons (append! (map (cut list <> #f #f) vars) (car e)) (cdr e)))
		(ev e))
	   (let loop1 ((vls vals) (vrs vars) (vals2 '()) (fv '()))
	     (if (null? vls)
		 (let ((body fv2 (walk body e2)))
		   (fixup! (take (length vars) (car e2)))
		   (values
		    `(let ,(let loop2 ((e2 (car e2)) (vals (reverse vals2)))
			     (if (eq? e2 e0)
				 '()
				 (cons 
				  (match (car e2)
				    ((v #t #t . _)
				     (let ((bo (box-op v)))
				       ;; now eliminate native type in vtlist, so that it will be treated as a generic value in CODEGEN
				       (cond ((assq v vtlist) => (cut set-cdr! <> '*)))
				       (list v (list bo (car vals)))))
				    ((v _ _ . _) (list v (car vals))))
				  (loop2 (cdr e2) (cdr vals)))))
		       ,body)
		    (difference (union fv fv2) vars)))
		 (let ((val fv2 (walk (car vls) ev)))
		   (loop1 (cdr vls) (cdr vrs) (cons val vals2) (union fv fv2)))))))
	(('set! v x)
	 (let ((x fv (walk x e)))
	   (cond ((and (pair? e) (assq v (car e))) => ; local
		  (lambda (a)
		    (let ((x2 `($local-set! ,v ,x)))
		      (set-car! (cdr a) #t) ; mark as assigned
		      (set-cdr! (cddr a) (cons x2 (cdddr a))) ; add to env-refs
		      (values x2 fv))))
		 ((and (pair? e) (any (cut assq v <>) (cdr e))) => ; lexical
		  (lambda (a)
		    (let ((x2 (list (box-set-op v) `($closure-ref ,v) x)))
		      (set-car! (cdr a) #t) ; mark as assigned
		      (set-car! (cddr a) #t) ; mark as shared
		      (set-cdr! (cddr a) (cons x2 (cdddr a))) ; add to env-refs
		      (values x2 (adjoin fv v)))))
		 (else
		  (values `($global-set! ,v ,x) fv)))))
	(('$lambda id llist body)
	 (let* ((vars argc rest (parse-lambda-list llist))
		(e2 (cons (filter-map
			   (lambda (var)
			     (and (not (eq? var '$unused))
				  (list var #f #f)))
			   vars)
			  e))
		(body fv (walk body e2)))
	   (let ((fv (difference fv vars)))
	     (fixup! (car e2))	       ; fixup closure-refs and boxing
	     (let ((fvrefs fv2 (mapwalk fv e)) ; create variable-references for closed-over variables
		   (ubs (filter-map
			 (match-lambda
			   ((v #t #t . _) (list v (list (box-op v) `($local-ref ,v))))
			   (_ #f))
			 (car e2))))
	       (values `($closure
			 ,id ,fvrefs 
			 (,llist
			  ,(if (null? ubs)
			       body
			       `(let ,ubs ,body))))
		       (union fv fv2))))))
	(('$case-lambda id (llists bodies) ...)
	 (let* ((total-fv '())
		(total-fvrefs '())
		(ll+bd
		 (map (lambda (llist body)
			(let* ((vars argc rest (parse-lambda-list llist))
			       (e2 (cons (filter-map
					  (lambda (var)
					    (and (not (eq? var '$unused))
						 (list var #f #f)))
					  vars)
					 e))
			       (body fv (walk body e2)))
			  (let ((fv (difference fv vars)))
			    (fixup! (car e2)) ; fixup closure-refs and boxing
			    (let ((fvrefs fv2 (mapwalk fv e)) ; create variable-references for closed-over variables
				  (ubs (filter-map
					(match-lambda
					  ((v #t #t . _) (list v (list (box-op v) `($local-ref ,v))))
					  (_ #f))
					(car e2))))
			      (set! total-fv (union total-fv fv fv2))
			      (set! total-fvrefs (union total-fvrefs fvrefs))
			      (list llist
				    (if (null? ubs)
					body
					`(let ,ubs ,body)))))))
		      llists bodies)))
	   (values `($closure ,id ,total-fvrefs ,@ll+bd) total-fv)))
	(('$call id args ...)
	 (let ((args fv (mapwalk args e)))
	   (values `($call ,id ,@args) fv)))
	((op args ...) (mapwalk x e))
	(_ (error "CC: invalid expression" x))))
    ;; now walk cc'd code and convert closure-ref'd names to indices
    (define (index-walk x cap)
      (match x
	((? symbol?) x)
	(('$closure-ref n) `($closure-ref ,(posq n cap)))
	(((or '$local-ref '$global-ref 'quote '$undefined '$uninitialized) . _) x)
	(((and head (or '$inline '$primitive 'set!)) n xs ...)
	 (cons* head n (map (cut index-walk <> cap) xs)))
	(('$inline-test n c xs ...)
	 `($inline-test ,n ,c ,@(map (cut index-walk <> cap) xs)))
	(('if xs ...)
	 `(if ,@(map (cut index-walk <> cap) xs)))
	(('let ((vars vals) ...) body)
	 `(let ,(map (lambda (var val) (list var (index-walk val cap))) vars vals)
	    ,(index-walk body cap)))
	(('$allocate t s args ...)
	 `($allocate ,t ,s ,@(map (cut index-walk <> cap) args)))
	(('$closure n fv (llists bodies) ...)
	 `($closure 
	   ,n
	   ,(map (lambda (v)
		   (match (index-walk v cap)
		     (((or '$word-box-ref '$float-box-ref '$box-ref) x) x) ; hack
		     (x x)))
		 fv)
	   ,@(map (lambda (llist body) (list llist (index-walk body (refs-vars fv)))) llists bodies)))
	(('$box x)
	 (inc! bcount)
	 `($box ,(index-walk x cap)))
	(('$word-box x) 
	 (inc! wbcount) 
	 `($word-box ,(index-walk x cap)))
	(('$float-box x) 
	 (inc! fbcount) 
	 `($float-box ,(index-walk x cap)))
	(('$call id args ...)
	 `($call ,id ,@(map (cut index-walk <> cap) args)))
	((op args ...)
	 (map (cut index-walk <> cap) x))
	(_ (error "CC: invalid expression" x))))
    (define (refs-vars refs)		; extracts variable names
      (map (match-lambda
	     (((or '$local-ref '$closure-ref) name) name)
	     (((or '$word-box-ref '$float-box-ref '$box-ref) ((or '$local-ref '$closure-ref) name)) name)
	     (ref (error "unknown ref" ref)))
	   refs))
    (let* ((form fv (walk form '()))
	   (form (index-walk form '())))
      (set! cc-box-counts (list bcount wbcount fbcount))
      form)))
