;;;; non-std extensions


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
	    (k (lambda () (apply values results))))))))))


(define (open-input-string str)
  (let ((data (cons (string-copy str) 0)))
    (%make-port 
     #t #f 
     (lambda (p)
       (set-car! data "")
       (set-cdr! data 0))
     (lambda (p n)
       (let* ((p1 (cdr data))
	      (s (car data))
	      (len (string-length s))
	      (p2 (%fx+ p1 n))
	      (p2 (if (%fx<=? p2 len) p2 len)))
	 (if (%eq? p1 p2)
	     (eof-object)
	     (let ((r (substring s p1 p2)))
	       (set-cdr! data p2)
	       r))))
     data)))

(define (open-output-string)
  (let ((data (cons (make-string 1024) 0)))
    (%make-port
     #f #f
     (lambda (p)
       (set-car! data "")
       (set-cdr! data 0))
     (lambda (p str)
       (let* ((p1 (cdr data))
	      (s (car data))
	      (n (string-length str))
	      (len (string-length s))
	      (p2 (%fx+ p1 n)))
	 (when (%fx>? p2 len)
	   (let ((new (make-string (%fx+ p2 5000))))
	     ($inline "CALL copy_bytes" (cons s 0) (cons new 0) p1)
	     (set! s new))
	   (set-car! data s))
	 (set-cdr! data p2)
	 ($inline "CALL copy_bytes" (cons str 0) (cons s p1) n)
	 str))
     data)))

(define (get-output-string port)
  (let* ((data (%slot-ref port 5))
	 (str (car data))
	 (p (cdr data)))
    (substring str 0 p)))


(cond-expand
  (time
   (define-inline (current-second) (%time)))
  (else))


(cond-expand
  (process-environment
   (define-inline (get-environment-variable str) (%getenv str))

   (define command-line
     (let* ((argc (%argc))
	    (lst (let loop ((i 0))
		   (if (%fx>=? i argc)
		       '()
		       (cons (%argv-ref i) (loop (%fx+ i 1)))))))
       (lambda () lst)))

   (define-inline (current-process-id) (%getpid))
   (define-inline (system str) (%system str)))

  (else))


(cond-expand
  (jiffy-clock
   (define-inline (current-jiffy) (%clock))
   (define-inline (jiffies-per-second) (%clocks-per-sec)))
  (else))


(define-syntax call/cc call-with-current-continuation)


(define (read-string n . p)
  (let* ((p (optional p %standard-input-port))
	 (pc (%slot-ref p 4))
	 (str ((%slot-ref p 3) p (if pc (%fx- n 1) n))))
    (if pc
	(string-append (%char->string pc) str)
	str)))

(define-syntax write-string
  (case-lambda
   ((str) ((%slot-ref %standard-output-port 3) %standard-output-port str))
   ((str p) ((%slot-ref p 3) p str))))

(cond-expand
  (file-system

   (define (current-directory . dir)
     (if (null? dir)
	 (or (%getcwd)
	     (%file-error 'current-directory))
	 (let ((r (%chdir (car dir))))
	   (when (%fx<? r 0)
	     (%file-error 'current-directory (car dir))))))

   (define-inline (delete-file str) 
     (when (%fx<? (%unlink str) 0)
       (%file-error 'delete-file str)))

   (define-inline (file-exists? str)
     (and (%exists? str) str)))

  (else))


(define reclaim ($primitive "reclaim_garbage"))


(cond-expand
  (file-ports
   (define (open-append-output-file name)
     (let ((fd (%open-append-file name)))
       (if (%fx<? fd 0)
	   (%file-error 'open-append-output-file name)
	   (%make-file-output-port fd)))))
  (else))


(define (print . args)
  (for-each display args)
  (newline)
  (void))

(define-inline (free) (%free))

(define (make-parameter val . guard)
  (let ((guard (optional guard (lambda (x) x)))
	(tag (%list #f)))
    (lambda args
      (let-optionals args ((new tag) (restore #f))
	(cond ((%eq? new tag) val)
	      (else
	       (set! val (if restore new (guard new)))
	       val))))))

(define %record-type-id-counter 2)	; 1 is used for error-objects

(define (make-disjoint-type . name)
  (let ((id %record-type-id-counter)
	(name (optional name 'record)))
    (set! %record-type-id-counter (%fx+ %record-type-id-counter 1))
    (values
     (lambda (data) ($allocate 10 3 name id data))
     (lambda (x) (and (record? x) (%eq? id (%slot-ref x 1))))
     (lambda (rec) (%slot-ref rec 2)))))


(define-inline (bytevector-u8-ref bv i) (%byte-ref bv i))
(define-inline (bytevector-u8-set! bv i n) (%byte-set! bv i n))
(define-inline (bytevector-length bv) (%size bv))

(define (bytevector . ns)
  (let* ((n (length ns))
	 (bv (%allocate-block #x12 n #f n #f #f)))
    (do ((i 0 (%fx+ i 1))
	 (ns ns (cdr ns)))
	((null? ns) bv)
      (%byte-set! bv i (car ns)))))

(define-syntax make-bytevector
  (case-lambda 
    ((n b) 
     (let ((bv (%allocate-block #x12 n #f n #f #f)))
       ($inline "CALL fill_bytes" bv b)
       bv))
    ((n) (%allocate-block #x12 n #f n #f #f))))

(define-syntax bytevector-copy!
  (case-lambda
    ((to at from start end)
     ($inline "CALL copy_bytes" (cons from start) (cons to at) (%fx- end start)))
    ((to at from start)
     ($inline "CALL copy_bytes" (cons from start) (cons to at) (%fx- (bytevector-length from) start)))
    ((to at from)
     ($inline "CALL copy_bytes" (cons from 0) (cons to at) (bytevector-length from)))))

(define (with-input-from-string str thunk)
  (parameterize ((current-input-port (open-input-string str)))
    (thunk)))

(define (with-output-to-string thunk)
  (parameterize ((current-output-port (open-output-string)))
    (thunk)
    (get-output-string (current-output-port))))
