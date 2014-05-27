(program
 (include "base.scm")
 (code

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
	   (tak (- y 1) z x)
	   (tak (- z 1) x y) ) ) )

(do ((i 1000 (- i 1))) ((zero? i)) 
  (let ((r (tak 18 12 6)))
    (unless (eq? 7 r)
      (display "failed.")
      (exit 1))))

))
