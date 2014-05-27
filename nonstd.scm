;;;; non-std extensions


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

(define (write-string str . p)
  (let ((p (optional p %standard-output-port)))
    ((%slot-ref p 3) p str)))

(cond-expand
  (file-system

   (define (current-directory . dir)
     (if (null? dir)
	 ($inline "CALL syscall_getcwd")	;XXX file-error
	 (let ((r ($inline "CALL syscall_chdir" (car dir))))
	   r)))				;XXX file-error

   (define-inline (delete-file str) ($inline "CALL syscall_delete_file" str))
   (define-inline (file-exists? str) (and ($inline "CALL syscall_file_exists" str) str)))

  (else))

(define reclaim ($primitive "reclaim_garbage"))

(cond-expand
  (time
   (define-inline (current-second) ($inline "CALL syscall_time")))
  (else))

(cond-expand
  (process-environment
   (define-inline (current-process-id) ($inline "CALL syscall_getpid"))
   (define-inline (get-environment-variable str) ($inline "CALL syscall_getenv" str))
   (define-inline (system str) ($inline "CALL syscall_shell_command" str))

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
   (define-inline (current-jiffy) ($inline "CALL syscall_clock"))
   (define-inline (jiffies-per-second) 1000000))
  (else))

(define-syntax call/cc call-with-current-continuation)

(cond-expand
  (file-ports
   (define (open-append-output-file name)
     (let ((fd ($inline "CALL syscall_open_append" name)))
       ;;XXX check for error
       (%make-file-output-port fd))))
  (else))

(define-inline (add1 x) (+ x 1))
(define-inline (sub1 x) (- x 1))

(define (print . args)
  (for-each display args)
  (newline))
