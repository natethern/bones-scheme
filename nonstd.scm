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


(define open-input-string
  (let ((substring substring))
    (lambda (str)
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
	     (if (eq? p1 p2)
		 (eof-object)
		 (let ((r (substring s p1 p2)))
		   (set-cdr! data p2)
		   r))))
	 data)))))

(define open-output-string
  (let ((make-string make-string))
    (lambda ()
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
	 data)))))

(define get-output-string
  (let ((substring substring))
    (lambda (port)
      (let* ((data (%slot-ref port 5))
	     (str (car data))
	     (p (cdr data)))
	(substring str 0 p)))))


(cond-expand
  (time
   (define-inline (current-second) ($inline "LIBCALL1 time, 0; INT2FIX rax")))
  (else))


(cond-expand
  (process-environment
   (define-inline (get-environment-variable str)
     ($inline 
      "CALL copy_to_buffer; LIBCALL1 getenv, buffer; test rax, rax; if z; mov rax, FALSE; endif; CALL alloc_zstring" 
      str)) )
  (else))

(cond-expand
  ((or process-environment linux-bare)
   (define command-line
     (let* ((argc (%argc))
	    (lst (let loop ((i 0))
		   (if (%fx>=? i argc)
		       '()
		       (cons (%argv-ref i) (loop (%fx+ i 1)))))))
       (lambda () lst))))
  (else))


(cond-expand
  (jiffy-clock
   (define-inline (current-jiffy) ($inline "LIBCALL0 clock; INT2FIX rax"))
   (define-inline (jiffies-per-second) 1000000))
  (else))


(define-syntax call/cc call-with-current-continuation)


(define read-string
  (let ((make-string make-string)
	(string-append string-append))
    (lambda (n . p)
      (let* ((p (optional p %standard-input-port))
	     (pc (%slot-ref p 4))
	     (str ((%slot-ref p 3) p (if pc (%fx- n 1) n))))
	(if pc
	    (string-append (%char->string pc) str)
	    str)))))

(define-syntax write-string
  (case-lambda
   ((str) ((%slot-ref %standard-output-port 3) %standard-output-port str))
   ((str p) ((%slot-ref p 3) p str))))

(cond-expand
  (file-system

   (define (current-directory . dir)
     (define (getcwd)
       (cond-expand
	 (linux-bare
	  ($inline "SYSCALL2 79, buffer, 1024; mov rax, buffer; CALL alloc_zstring"))
	 (else
	  ($inline "LIBCALL2 getcwd, buffer, 1024; mov rax, buffer; CALL alloc_zstring"))))
     (define (chdir dir)
       (cond-expand
	 (linux-bare
	  ($inline "CALL copy_to_buffer; SYSCALL1 80, buffer; INT2FIX rax" dir))
	 (else
	  ($inline "CALL copy_to_buffer; LIBCALL1 chdir, buffer; INT2FIX rax" dir))))
     (if (null? dir)
	 (if (%fx<? (getcwd) 0)
	     (%file-error 'current-directory)
	     (let ((r (chdir (car dir))))
	       (when (%fx<? r 0)
		 (%file-error 'current-directory (car dir)))))))

   (cond-expand
     (linux-bare
      (define-inline (delete-file str) 
	(when (%fx<? ($inline "CALL copy_to_buffer; SYSCALL1 87, buffer; INT2FIX rax" str) 0)
	  (%file-error 'delete-file str)))
      (define-inline (file-exists? str)
	(and
	 ($inline "CALL copy_to_buffer; SYSCALL2 4, buffer, stat_buffer; test rax, rax; SET_T rax; cmovnz rax, FALSE" str) 
	 str)))

     (else
      (define-inline (delete-file str) 
	(when (%fx<? ($inline "CALL copy_to_buffer; LIBCALL1 unlink, buffer; INT2FIX rax" str) 0)
	  (%file-error 'delete-file str)))
      (define-inline (file-exists? str)
	(and
	 ($inline "CALL copy_to_buffer; LIBCALL2 stat, buffer, stat_buffer; test rax, rax; SET_T rax; cmovnz rax, FALSE" str) 
	 str)))))

  (else))


(define reclaim ($primitive "reclaim_garbage"))


(cond-expand
  (linux-bare
   (define-inline (current-process-id) ($inline "SYSCALL0 39; INT2FIX rax")))
  (process-environment
   (define-inline (current-process-id) ($inline "LIBCALL0 getpid; INT2FIX rax"))
   (define-inline (system str)
     ($inline "CALL copy_to_buffer; LIBCALL1 system, buffer; INT2FIX rax" str)))
  (else))

(cond-expand
  (process-environment
   (define-inline (current-process-id) ($inline "LIBCALL0 getpid; INT2FIX rax"))

   (define-inline (system str)
     ($inline "CALL copy_to_buffer; LIBCALL1 system, buffer; INT2FIX rax" str)))

  (else))


(cond-expand
  (file-ports
   (define (open-append-output-file name)
     ;; open-flags: O_WRONLY|O_CREAT|O_APPEND, mode: S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH
     (define (open name)
       (cond-expand
	 (linux-bare 
	  ($inline "CALL copy_to_buffer; SYSCALL3 1, buffer, 1089, 420; INT2FIX rax" name))
	 (else
	  ($inline "CALL copy_to_buffer; LIBCALL3 open, buffer, 1089, 420; INT2FIX rax" name))))
     (let ((fd (open name)))
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
	(cond ((eq? new tag) val)
	      (else
	       (set! val (if restore new (guard new)))
	       val))))))
