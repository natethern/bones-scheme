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
	 ((_ "1" (slot-name ...) (proc arg ...))
	  (lambda (slot-name ...) (proc arg ...)))
	 ((_ "1" (slot-name ...) (proc arg ...) <...>)
	  (lambda (slot-name ... . rest-slot) (apply proc arg ... rest-slot)))
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
		 (k (lambda () (apply values results))))))))))
     (define-syntax cond-expand
       (syntax-rules (and or not else si)
	 ((cond-expand) (error 'cond-expand "no matching clause"))
	 ((cond-expand (si body ...) . more-clauses)
	  (begin body ...))
	 ((cond-expand (else body ...)) (begin body ...))
	 ((cond-expand ((and) body ...) more-clauses ...) (begin body ...))
	 ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
	  (cond-expand
	    (req1 (cond-expand
		    ((and req2 ...) body ...)
		    more-clauses ...))
	    more-clauses ...))
	 ((cond-expand ((or) body ...) more-clauses ...) 
	  (cond-expand more-clauses ...))
	 ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
	  (cond-expand
	    (req1 (begin body ...))
	    (else
	     (cond-expand
	       ((or req2 ...) body ...)
	       more-clauses ...))))
	 ((cond-expand ((not req) body ...) more-clauses ...)
	  (cond-expand
	    (req (cond-expand more-clauses ...))
	    (else body ...)))
	 ((cond-expand (feature-id body ...) more-clauses ...)
	  (cond-expand more-clauses ...))))))


(define eval-unbound-value (list 'unbound))
(define eval-potentially-unbound #f)
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
     current-process-id command-line
     system
     read-string write-string
     print
     reclaim free
     file-exists? delete-file
     current-directory
     open-append-output-file
     make-parameter make-disjoint-type
     expand
     bytevector bytevector? bytevector-length bytevector-u8-ref bytevector-u8-set!
     make-bytevector bytevector-copy!
     interrupt? interrupt-number catch-interrupt check-interrupts)))


(define eval-trace (make-parameter #f))
(define eval-trace-buffer '())
(define eval-trace-buffer-end '())
(define eval-trace-buffer-len 0)
(define eval-trace-buffer-max 20)

(define (fragment exp maxdepth)
  (define (walk x d)
    (if (> d maxdepth)
	'...
	(cond ((vector? x) (list->vector (walk (vector->list x) d)))
	      ((pair? x)
	       (let loop ((x x) (n maxdepth))
		 (cond ((null? x) '())
		       ((zero? n) '(...))
		       ((pair? x) (cons (walk (car x) (+ d 1)) (loop (cdr x) (- n 1))))
		       (else x))))
	      (else x))))
  (walk exp 1))

(define (eval-print-trace-buffer . port)
  (when (pair? eval-trace-buffer)
    (let ((out (optional port (current-error-port))))
      (display "\nCall trace:\n" out)
      (for-each
       (lambda (exp)
	 (display "\n  " out)
	 (write (fragment exp 10) out) )
       eval-trace-buffer)
      (display "   <---\n\n" out))))


(define (eval x)
  (let ((evtrace (eval-trace)))

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

    (define (trace exp)
      (cond ((not evtrace))
	    ((eq? eval-trace-buffer-len 0)
	     (set! eval-trace-buffer-len 1)
	     (set! eval-trace-buffer (list exp))
	     (set! eval-trace-buffer-end eval-trace-buffer))
	    (else
	     (if (eq? eval-trace-buffer-len eval-trace-buffer-max)
		 (set! eval-trace-buffer (cdr eval-trace-buffer))
		 (set! eval-trace-buffer-len (%fx+ eval-trace-buffer-len 1)))
	     (let ((n (list exp)))
	       (set-cdr! eval-trace-buffer-end n)
	       (set! eval-trace-buffer-end n)))))

    (define (compile x e)
      (match x

	((? symbol?)
	 (cond ((lookup x e) =>
		(match-lambda 
		  ((0 . j)
		   (lambda (v)
		     (vector-ref (car v) j)))
		  ((i . j)
		   (lambda (v)
		     (vector-ref (list-ref v i) j)))))
	       (else
		(let ((cell (findcell x)))
		  ;; no need to check bound-ness if already bound
		  (cond ((eq? (cdr cell) eval-unbound-value)
			 (when (and eval-potentially-unbound
				    (not (assq x eval-potentially-unbound)))
			   (set! eval-potentially-unbound (cons cell eval-potentially-unbound)))
			 (lambda (v)
			   (let ((val (cdr cell)))
			     (if (eq? val eval-unbound-value)
				 (error "unbound variable" x)
				 val))))
			(else (lambda (v) (cdr cell))))))))

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
		    ((0 . j)
		     (lambda (v)
		       (vector-set! (car v) j (x v))
		       (void)))
		    ((i . j)
		     (lambda (v)
		       (vector-set! (list-ref v i) j (x v))
		       (void)))))
		 (else
		  (let ((cell (findcell var)))
		    (lambda (v)
		      (set-cdr! cell (x v))
		      (void)))))))

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
	   (lambda (v) 
	     (set-cdr! cell (x v))
	     (void))))

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
			(lambda r
			  (%interrupt-hook)
			  (body (cons (vector r) v))))
		      (lambda (v) 
			(lambda () (body v)))))
		 ((1)
		  (if rest
		      (lambda (v)
			(lambda (a . r)
			  (%interrupt-hook)
			  (body (cons (vector a r) v))))
		      (lambda (v)
			(lambda (a)
			  (%interrupt-hook)
			  (body (cons (vector a) v))))))
		 ((2)
		  (if rest
		      (lambda (v)
			(lambda (a1 a2 . r)
			  (%interrupt-hook)
			  (body (cons (vector a1 a2 r) v))))
		      (lambda (v)
			(lambda (a1 a2)
			  (%interrupt-hook)
			  (body (cons (vector a1 a2) v))))))
		 ((3)
		  (if rest
		      (lambda (v)
			(lambda (a1 a2 a3 . r)
			  (%interrupt-hook)
			  (body (cons (vector a1 a2 a3 r) v))))
		      (lambda (v)
			(lambda (a1 a2 a3)
			  (%interrupt-hook)
			  (body (cons (vector a1 a2 a3) v))))))
		 ((4)
		  (if rest
		      (lambda (v)
			(lambda (a1 a2 a3 a4 . r)
			  (%interrupt-hook)
			  (body (cons (vector a1 a2 a3 a4 r) v))))
		      (lambda (v)
			(lambda (a1 a2 a3 a4)
			  (%interrupt-hook)
			  (body (cons (vector a1 a2 a3 a4) v))))))
		 (else
		  (if rest
		      (lambda (v)
			(lambda args
			  (%interrupt-hook)
			  (body (cons (list->vector/rest args argc) v))))
		      (lambda (v)
			(lambda args
			  (%interrupt-hook)
			  (body (cons (list->vector args) v)))))))))))

	(('$case-lambda ('lambda llists . bodies) ...)
	 ;;XXX this can probably be done more efficiently
	 (let ((bodies (map (lambda (llist body)
			      (compile `(lambda ,llist ,@body) e))
			    llists bodies))
	       (tests (map (lambda (llist)
			     (call-with-values (cut parse-lambda-list llist)
			       (lambda (vars argc rest)
				 (lambda (args)
				   (let loop ((i 0) (args args))
				     (cond ((>= i argc) (or rest (null? args)))
					   ((null? args) #f)
					   (else (loop (+ i 1) (cdr args)))))))))
			   llists)))
	   (lambda (v)
	     (lambda args
	       (%interrupt-hook)
	       (let loop ((tests tests) (bodies bodies))
		 (cond ((null? tests) (error 'case-lambda "no matching case" args))
		       (((car tests) args) (apply ((car bodies) v) args))
		       (else (loop (cdr tests) (cdr bodies)))))))))

	((op args ...)
	 (let ((n (length x))
	       (y (map (cut compile <> e) x)))
	   (case n
	     ((1)
	      (let ((op (car y)))
		(lambda (v) (trace x) ((op v)))))
	     ((2)
	      (let ((op (car y))
		    (a1 (cadr y)))
		(lambda (v) (trace x) ((op v) (a1 v)))))
	     ((3)
	      (let ((op (car y))
		    (a1 (cadr y))
		    (a2 (caddr y)))
		(lambda (v) (trace x) ((op v) (a1 v) (a2 v)))))
	     ((4)
	      (let ((op (car y))
		    (a1 (cadr y))
		    (a2 (caddr y))
		    (a3 (cadddr y)))
		(lambda (v) (trace x) ((op v) (a1 v) (a2 v) (a3 v)))))
	     (else
	      (let ((op (car y))
		    (args (cdr y)))
		(lambda (v)
		  (trace x)
		  (apply (op v) (map (lambda (a) (a v)) args))))))))

	(_ (error "invalid expression" x))))

    ((compile (expand-syntax x) '()) '())))


(define-values (load load-verbose)
  (let ((home (get-environment-variable "HOME")))
    (define (load filename evproc verbose)
      (let* ((filename (if (and (positive? (string-length filename))
				(char=? #\~ (string-ref filename 0)))
			   (string-append home (substring filename 1))
			   filename))
	     (in (open-input-file filename)))
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
		    (loop))))
	      (void))
	    (cut close-input-port in))))
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

(define (eval-quit-hook result) (exit))

(define eval-repl-level 0)

(define (quit . result) (eval-quit-hook (optional result (void))))

(define repl-prompt 
  (make-parameter 
   (lambda () (string-append (make-string eval-repl-level #\>) " "))))

(define repl-print (make-parameter (lambda (x) (write x) (newline))))

(define (repl)
  (define (report-unbound)
    (let loop ((cells eval-potentially-unbound) (ub '()))
      (match cells
	(() 
	 (when (pair? ub)
	   (let ((out (current-error-port)))
	     (display "Warning: the following global variables are currently unbound:\n\n" out)
	     (for-each
	      (lambda (a)
		(display "  " out)
		(display (car a) out)
		(newline out))
	      ub)
	     (newline out))))
	(((and a (_ . val)) . more)
	 (loop more (if (eq? eval-unbound-value val) (cons a ub) ub))))))
  (let ((rpt (repl-prompt))
	(rp (repl-print)))
    (call/cc
     (lambda (exit)
       (fluid-let ((eval-potentially-unbound eval-potentially-unbound)
		   (eval-quit-hook
		    (case-lambda 
		      (() (exit (void)))
		      ((result) (exit result))))
		   (eval-repl-level (+ eval-repl-level 1)))
	 (do () (#f)
	   (display (rpt))
	   (call/cc
	    (lambda (return)
	      (parameterize ((current-exception-handler
			      (lambda (exn)
				(let ((out (current-error-port)))
				  (newline out)
				  (cond ((error-object? exn)
					 (display "Error: " out)
					 (display (error-object-message exn) out)
					 (newline out)
					 (for-each
					  (lambda (x)
					    (newline out)
					    (write (fragment x 10) out)
					    (newline out))
					  (error-object-irritants exn)))
					(else
					 (display "Unhandled excception: " out)
					 (write exn out)
					 (newline out)))
				  (when (eval-trace) (eval-print-trace-buffer))
				  (return #f)))))
		(set! eval-trace-buffer '())
		(set! eval-trace-buffer-end '())
		(set! eval-trace-buffer-len 0)
		(set! eval-potentially-unbound '())
		(let ((x (read)))
		  (when (eof-object? x) (exit #f))
		  (call-with-values 
		      (lambda ()
			(begin0
			  (eval x)
			  (report-unbound)))
		    (lambda results
		      (unless (and (= 1 (length results))
				   (eq? (void) (car results)))
			(for-each rp results)))))))))
	 (newline))))))

(eval
 `(begin
    (define load-verbose ',load-verbose)
    (define load ',load)
    (define quit ',quit)
    (define repl ',repl)
    (define eval ',eval)
    (define repl-prompt ',repl-prompt)
    (define repl-print ',repl-print)
    (define oblist ',(lambda () eval-environment))))

)
