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


;;XXX is this R7RS? extend, is needed
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

;;XXX
(define-syntax write-string
  (case-lambda
   ((str) ((%slot-ref %standard-output-port 3) %standard-output-port str))
   ((str p) ((%slot-ref p 3) p str))))

(cond-expand
  (file-system

   (define (current-directory . dir)
     (if (null? dir)
	 ($inline "LIBCALL2 getcwd, buffer, 1024; mov rax, buffer; CALL alloc_zstring")	;XXX file-error
	 (let ((r ($inline "CALL copy_to_buffer; LIBCALL1 chdir, buffer; INT2FIX rax" (car dir))))
	   r)))				;XXX file-error

   (define-inline (delete-file str) 
     ($inline "CALL copy_to_buffer; LIBCALL1 unlink, buffer; INT2FIX rax" str))

   (define-inline (file-exists? str)
     (and
      ($inline "CALL copy_to_buffer; LIBCALL2 stat, buffer, stat_buffer; test rax, rax; SET_T rax; cmovnz rax, FALSE" str) 
      str)))

  (else))


(define reclaim ($primitive "reclaim_garbage"))


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
     (let ((fd ($inline "CALL copy_to_buffer; LIBCALL3 open, buffer, 1089, 420; INT2FIX rax" name)))
       ;;XXX check for error
       (%make-file-output-port fd))))
  (else))


(define (print . args)
  (for-each display args)
  (newline)
  (void))

(define-inline (free) (%free))

(define return-to-host ($primitive "return_to_host"))
