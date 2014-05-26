
(program
 (include "base.scm")
 (code

  (define (fac n)
    (if (zero? n)
	1
	(* n (fac (- n 1)))))

  (write-string (number->string (fac 10)))
  (newline)))
