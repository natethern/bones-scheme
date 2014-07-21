;;;; simple mangling to C identifiers
;
; - taken from grass-libs, but needs to notice when non-ASCII characters appear in literals.


(define (mangle-identifier x)
  (apply
   string-append
   "___"
   (map (lambda (c)
	  (if (or (char-lower-case? c)
		  (char-numeric? c))
	      (string c)
	      (let* ((n (char->integer c))
		     (s (number->string n 16)))
		(string-append
		 "_"
		 (if (< n 128)
		     (padl s 2 #\0)
		     (string-append "U" (padl s 5 #\0)))))))
	(string->list (stringify x)))))

(define (mangle-string-constant str)
  (string-append
   "\""
   (with-output-to-string
     (lambda ()
       (for-each
	(lambda (c) 
	  (let ((n (char->integer c)))
	    (cond ((< n 128)
		   (emit "\\" (padl (number->string (char->integer c) 8) 3 #\0)))
		  (else
		   (emit "\\x" (number->string (char->integer c) 16) "\"\"")))))
	(string->list str))))
   "\""))

(define (mangle-feature-name name)
  (string-append
   "FEATURE_"
   (list->string 
    (map (lambda (c)
	   (case c
	     ((#\-) #\_)
	     (else (char-upcase c))))
	 (string->list (symbol->string name))))))
