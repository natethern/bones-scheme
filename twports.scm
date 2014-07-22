;;;; custom ports for tw.c


(define topline '())
(define width 0)
(define height 0)
(define cursor-row 0)
(define cursor-column 0)


(define (%make-tw-input-port)
  (let ((closed #f))
    (define (read1)
      (match (return-to-host topline)
	(#(c ks state 0)
	 (decode-key c ks state))
	(#(0 _ _ _)			; close
	 (set! closed #t)
	 (eof-object))
	(#(1 w h _)			; resize
	 (set! width w)
	 (set! height h))
	(x (error "invalid event" x))))
    (%make-port 
     #t #f void
     (lambda (p n) 
       (if closed
	   (eof-object)
	   (let ((str (make-string n)))
	     (let loop ((i 0))
	       (let ((c (read1)))
		 (cond (closed 
			(if (zero? i)
			    (eof-object)
			    (substring str 0 i)))
		       ((>= i n) str)
		       (else 
			(string-set! str i c)
			(loop (add1 i)))))))))
   #f))

(define (%make-tw-output-port)
  (%make-port 
   #f #f void
   (lambda (p str)
     (insert-text str))
   #f))
