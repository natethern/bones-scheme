;;;; a simple evaluator



(files "alexpand.scm" "match.scm")

(code

 (expand-syntax
  '(begin
     (define-syntax define-syntax-rule
       (syntax-rules
	   ___ ()
	   ((_ (name args ___) rule)
	    (define-syntax name
	      (syntax-rules ()
		((_ args ___) rule))))))
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
     (define-syntax cut
       (syntax-rules (<> <...>)
	 ;; construct fixed- or variable-arity procedure:
	 ((_ "1" (slot-name ...) (proc arg ...))
	  (lambda (slot-name ...) (proc arg ...)))
	 ((_ "1" (slot-name ...) (proc arg ...) <...>)
	  (lambda (slot-name ... . rest-slot) (apply proc arg ... rest-slot)))
	 ;; process one slot-or-expr
	 ((_ "1" (slot-name ...)   (position ...)      <>  . se)
	  (cut "1" (slot-name ... x) (position ... x)        . se))
	 ((_ "1" (slot-name ...)   (position ...)      nse . se)
	  (cut "1" (slot-name ...)   (position ... nse)      . se))
	 ((_ . slots-or-exprs)
	  (cut "1" () () . slots-or-exprs))) )
     (define-syntax fluid-let
       (syntax-rules ()
	 ((_ ((v1 e1) ...) b1 b2 ...)
	  (fluid-let "temps" () ((v1 e1) ...) b1 b2 ...))
	 ((_ "temps" (t ...) ((v1 e1) x ...) b1 b2 ...)
	  (let ((temp e1))
	    (fluid-let "temps" ((temp e1 v1) t ...) (x ...) b1 b2 ...)))
	 ((_ "temps" ((t e v) ...) () b1 b2 ...)
	  (let-syntax ((swap!
			(syntax-rules ()
			  ((swap! a b)
			   (let ((tmp a))
			     (set! a b)
			     (set! b tmp))))))
	    (dynamic-wind
		(lambda () (swap! t v) ...)
		(lambda () b1 b2 ...)
		(lambda () (swap! t v) ...))))))
     (define-syntax-rule (begin0 x1 x2 ...)
       (call-with-values (lambda () x1)
	 (lambda results
	   x2 ...
	   (apply values results))))
     (define-syntax let-optionals
       (syntax-rules ()
	 ((_ rest () body ...) (let () body ...))
	 ((_ rest ((var default) . more) body ...)
	  (let* ((tmp rest)
		 (var (if (null? tmp) default (car tmp)))
		 (rest2 (if (null? tmp) '() (cdr tmp))) )
	    (let-optionals rest2 more body ...) ) )
	 ((_ rest (var) body ...) (let ((var rest)) body ...)) ) )
     (define-syntax assert
       (syntax-rules ()
	 ((_ x) (assert x "assertion failed" 'x))
	 ((_ x args ...) 
	  (let ((tmp x))
	    (unless tmp (error args ...))
	    tmp))))
     (define-syntax define-values
       (syntax-rules ()
	 ((_ "1" () exp ((var tmp) ...))
	  (define
	    (call-with-values (lambda () exp)
	      (lambda (tmp ...)
		(set! var tmp) ...))))
	 ((_ "1" (var . more) exp (binding ...))
	  (define-values "1" more exp (binding ... (var tmp))))
	 ((_ () exp) 
	  (define 
	    (call-with-values (lambda () exp)
	      (lambda _ (void)))))
	 ((_ (var) exp) 
	  (define var exp))
	 ((_ (var ...) exp)
	  (begin
	    (define var #f) ...
	    (define-values "1" (var ...) exp ())))))
     (define-syntax parameterize
       (letrec-syntax ((bind-param 
			(syntax-rules ()
			  ((_ () (param ...) (new ...) (old ...) body)
			   (dynamic-wind
			       (lambda () 
				 (param new) ...)
			       (lambda () body)
			       (lambda ()
				 (param old #t) ...)))
			  ((_ ((name val) . more) (param ...) (new ...) (old ...) body)
			   (let* ((newname name)
				  (newval val)
				  (oldval (newname)))
			     (bind-param
			      more
			      (param ... newname)
			      (new ... newval)
			      (old ... oldval)
			      body))))))
	 (syntax-rules ()
	   ((_ bindings body ...)
	    (bind-param bindings () () () (begin body ...))))))
     (define-syntax-rule (handle-exceptions var handler body ...)
       ((call-with-current-continuation
	 (lambda (k)
	   (parameterize ((current-exception-handler
			   (lambda (var) (k (lambda () handler)))))
	     (call-with-values (lambda () body ...)
	       (lambda results
		 (k (lambda () (apply values results))))))))))))

(define eval-unbound-value (list 'unbound))
(define expand expand-syntax)

(define eval-environment
  (let-syntax ((primitives
		(syntax-rules ()
		  ((_ name ...)
		   (list (cons 'name name) ...)))))
    (primitives
     void list 
     %make-promise 			; expansion from alexpand.scm
     car cdr caar cadr cdar cddr
     caaar caadr cadar caddr cdaar cdadr cddar cdddr
     caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
     set-car! set-cdr!
     not
     eq? eqv? equal? 
     pair? null? symbol? string? procedure? vector? char? eof-object? boolean? record? input-port? output-port? 
     port? promise? number? real? exact? inexact? rational?
     char->integer integer->char
     + - * / = > < >= <=
     bitwise-ior bitwise-and bitwise-xor bitwise-not arithmetic-shift
     negative? positive? zero? even? odd? finite? nan? integer?
     exact->inexact inexact->exact 
     quotient remainder modulo abs 
     sin cos tan asin acos atan log sqrt expt exp
     cons length 
     gcd lcm
     max min
     list?
     string-length vector-length
     list-tail list-ref
     reverse append
     string-ref string-set!
     apply
     raise
     current-input-port current-output-port current-error-port
     open-input-file open-output-file
     close-input-port close-output-port
     write-char newline read-char peek-char
     vector-ref vector-set! 
     char-upcase char-downcase 
     char-alphabetic? char-whitespace? char-numeric? char-upper-case? char-lower-case?
     char=? char>? char<? char>=? char<=?
     char-ci=? char-ci>? char-ci<? char-ci>=? char-ci<=?
     string=? string>? string<? string>=? string<=?
     string-ci=? string-ci>? string-ci<? string-ci>=? string-ci<=?
     string-fill! make-string string-append string-copy substring
     string->number number->string
     vector-fill! make-vector list->vector vector->list 
     list->string string->list
     vector string
     symbol->string string->symbol
     for-each map
     assq assv assoc
     memq memv member
     values call-with-values
     call-with-current-continuation call/cc
     dynamic-wind
     display write
     emergency-exit exit
     error-object? error-object-message error-object-irritants error-object-location
     current-exception-handler error
     file-error? read-error?
     read case-sensitive
     call-with-input-file call-with-output-file
     with-input-from-file with-output-to-file
     force
     truncate round floor ceiling
     open-input-string open-output-string get-output-string
     with-input-from-string with-output-to-string
     current-second
     get-environment-variable
     current-jiffy jiffies-per-second
     current-process-id 
     system
     read-string write-string
     print
     reclaim free
     open-append-output-file
     make-parameter make-disjoint-type
     expand
     bytevector bytevector? bytevector-length bytevector-u8-ref bytevector-u8-set!
     make-bytevector bytevector-copy!)))


(define (eval x)

  (define (posq x lst)
    (let loop ((i 0) (lst lst))
      (cond ((null? lst) #f)
	    ((eq? x (car lst)) i)
	    (else (loop (+ i 1) (cdr lst))))))

  (define (lookup var e)
    (let loop ((i 0) (e e))
      (cond ((null? e) #f)
	    ((posq var (car e)) => (cut cons i <>))
	    (else (loop (+ i 1) (cdr e))))))

  (define (findcell var)
    (or (assq var eval-environment) 
	(let ((a (cons var eval-unbound-value)))
	  (set! eval-environment (cons a eval-environment))
	  a)))

  (define (parse-lambda-list llist)	; -> vars argc rest
    (let loop ((ll llist) (vars '()))
      (cond ((null? ll) 
	     (values (reverse vars) (length vars) #f))
	    ((symbol? ll) 
	     (values (reverse (cons ll vars)) (length vars) ll))
	    ((pair? ll) 
	     (loop (cdr ll) (cons (car ll) vars)))
	    (else (error "invalid lambda-list" llist)))))

  (define (list->vector/rest args argc)
    (let ((vec (make-vector (+ argc 1))))
      (do ((i 0 (+ i 1))
	   (args args (cdr args)))
	  ((>= i argc) 
	   (vector-set! vec i args)
	   vec)
	(vector-set! vec i (car args)))))

  (define (compile x e)
    (match x

      ((? symbol?)
       (cond ((lookup x e) =>
	      (match-lambda 
		((i . j)
		 (lambda (v)
		   (vector-ref (list-ref v i) j)))))
	     (else
	      (let ((cell (findcell x)))
		;; no need to check bound-ness if already bound
		(if (eq? (cdr cell) eval-unbound-value)
		    (lambda (v)
		      (let ((val (cdr cell)))
			(if (eq? val eval-unbound-value)
			    (error "unbound variable" x)
			    val)))
		    (let ((val (cdr cell)))
		      (lambda (v) val)))))))

      ((or (? number?)
	   (? string?)
	   (? char?)
	   (? vector?)
	   (? bytevector?)
	   (? boolean?))
       (lambda (v) x))

      (('quote c) 
       (lambda (v) c))

      (('$uninitialized) ; produced by alexpand's expansion of "letrec"
       (lambda (v) (void)))

      (('set! var x)
       (let ((x (compile x e)))
	 (cond ((lookup var e) =>
		(match-lambda 
		  ((i . j)
		   (lambda (v) (vector-set! (list-ref v i) j (x v))))))
	       (else
		(let ((cell (findcell var)))
		  (lambda (v) (set-cdr! cell (x v))))))))

      (('if x y)
       (let ((x (compile x e))
	     (y (compile y e)))
	 (lambda (v)
	   (if (x v) (y v)))))

      (('if x y z)
       (let ((x (compile x e))
	     (y (compile y e))
	     (z (compile z e)))
	 (lambda (v)
	   (if (x v) (y v) (z v)))))

      (('begin) void)

      (('begin x) (compile x e))

      (('begin x more ...)
       (let ((x (compile x e))
	     (more (compile `(begin ,@more) e)))
	 (lambda (v)
	   (x v)
	   (more v))))

      (('define var x)
       (let* ((x (compile x e))
	      (cell (findcell var)))
	 (lambda (v) (set-cdr! cell (x v)))))

      (('letrec* ((vars vals) ...) body ...)
       (let* ((e (cons vars e))
	      (vals (map (cut compile <> e) vals))
	      (body (compile `(begin ,@body) e))
	      (n (length vars)))
	 (lambda (v)
	   (let* ((v0 (make-vector n (void)))
		  (v (cons v0 v)))
	     (do ((i 0 (+ i 1))
		  (vals vals (cdr vals)))
		 ((>= i n))
	       (vector-set! v0 i ((car vals) v)))
	     (body v)))))

      (('lambda llist body ...)
       (call-with-values (cut parse-lambda-list llist)
	 (lambda (vars argc rest)
	   (let* ((e (if (null? vars) e (cons vars e)))
		  (body (compile `(begin ,@body) e)))
	     (case argc
	       ((0) 
		(if rest
		    (lambda (v)
		      (lambda r (body (cons (vector r) v))))
		    (lambda (v) 
		      (lambda () (body v)))))
	       ((1)
		(if rest
		    (lambda (v)
		      (lambda (a . r) (body (cons (vector a r) v))))
		    (lambda (v)
		      (lambda (a) (body (cons (vector a) v))))))
	       ((2)
		(if rest
		    (lambda (v)
		      (lambda (a1 a2 . r) (body (cons (vector a1 a2 r) v))))
		    (lambda (v)
		      (lambda (a1 a2) (body (cons (vector a1 a2) v))))))
	       ((3)
		(if rest
		    (lambda (v)
		      (lambda (a1 a2 a3 . r) (body (cons (vector a1 a2 a3 r) v))))
		    (lambda (v)
		      (lambda (a1 a2 a3) (body (cons (vector a1 a2 a3) v))))))
	       ((4)
		(if rest
		    (lambda (v)
		      (lambda (a1 a2 a3 a4 . r) (body (cons (vector a1 a2 a3 a4 r) v))))
		    (lambda (v)
		      (lambda (a1 a2 a3 a4) (body (cons (vector a1 a2 a3 a4) v))))))
	       (else
		(if rest
		    (lambda (v)
		      (lambda args
			(body (cons (list->vector/rest args argc) v))))
		    (lambda (v)
		      (lambda args
			(body (cons (list->vector args) v)))))))))))

      ((op args ...)
       (let ((n (length x))
	     (x (map (cut compile <> e) x)))
	 (case n
	   ((1)
	    (let ((op (car x)))
	      (lambda (v) ((op v)))))
	   ((2)
	    (let ((op (car x))
		  (a1 (cadr x)))
	      (lambda (v) ((op v) (a1 v)))))
	   ((3)
	    (let ((op (car x))
		  (a1 (cadr x))
		  (a2 (caddr x)))
	      (lambda (v) ((op v) (a1 v) (a2 v)))))
	   ((4)
	    (let ((op (car x))
		  (a1 (cadr x))
		  (a2 (caddr x))
		  (a3 (cadddr x)))
	      (lambda (v) ((op v) (a1 v) (a2 v) (a3 v)))))
	   (else
	    (let ((op (car x))
		  (args (cdr x)))
	      (lambda (v)
		(apply (op v) (map (lambda (a) (a v)) args))))))))

      (_ (error "invalid expression" x))))

  ((compile (expand-syntax x) '()) '()))


(define-values (load load-verbose)
  (let ((load
	 (lambda (filename evproc verbose)
	   (let ((in (open-input-file filename)))
	     (dynamic-wind
		 void
		 (lambda ()
		   (let loop ()
		     (let ((x (read in)))
		       (unless (eof-object? x)
			 (when verbose 
			   (newline)
			   (write x)
			   (newline))
			 (evproc x)
			 (loop)))))
		 (cut close-input-port in))))))
    (values
     (case-lambda
       ((filename ev) (load filename ev #f))
       ((filename) (load filename eval #f)))
     (case-lambda
       ((filename ev) (load filename ev #t))
       ((filename) (load filename eval #t))))))


(cond-expand
  (embedded (eval `(define return-to-host ',return-to-host)))
  (else))

(eval
 `(begin
    (define load-verbose ',load-verbose)
    (define load ',load)
    (define oblist ',(lambda () eval-environment))))

(define (repl)
  (define (eval-form x return)
    (parameterize ((current-exception-handler
		    (lambda (exn)
		      (let ((out (current-error-port)))
			(newline out)
			(cond ((error-object? exn)
			       (display "Error: " out)
			       (display (error-object-message exn) out)
			       (for-each
				(lambda (x)
				  (newline out)
				  (write x out)
				  (newline out))
				(error-object-irritants exn)))
			      (else
			       (display "Unhandled excception: " out)
			       (write exn out)
			       (newline out)))
			(return #f)))))
      (eval x)))
  (call/cc
   (lambda (exit)
     (do () (#f)
       (display "> ")
       (let ((x (read)))
	 (when (eof-object? x) (exit #f))
	 (call/cc
	  (lambda (return)
	    (call-with-values (cut eval-form x return)
	      (lambda results
		(for-each
		 (lambda (x)
		   (write x) 
		   (newline))
		 results)))))))
     (newline))))

)
