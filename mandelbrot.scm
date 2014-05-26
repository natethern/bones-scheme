
(program
 (include "base.scm")
 (code
  
;;; The Computer Language Benchmarks Game
;;; http://shootout.alioth.debian.org/
;;;
;;; contributed by Anthony Borla


(define +limit-sqr+ 4.0)

(define +iterations+ 50)

(define (mandelbrot iterations x y n)
  (let ((cr (- (/ (* 2.0 x) n) 1.5)) (ci (- (/ (* 2.0 y) n) 1.0)))
    (let loop ((i 0) (zr 0.0) (zi 0.0))
      (let ((zrq (* zr zr)) (ziq (* zi zi)))
        (cond
          ((> i iterations) 1)
          ((> (+ zrq ziq) +limit-sqr+) 0)
          (else (loop (+ 1 i) (+ (- zrq ziq) cr) (+ (* 2.0 zr zi) ci)))) ))))

(define (main args)
  (let ((n (if (null? args)
               1
               (string->number (car args))))
	(bitnum 0)
	(byteacc 0)
	(out (open-output-string)))
    (display (string-append "P4\n" (number->string n) " " (number->string n)) out)
    (newline out)
    (let loop-y ((y 0))
      (unless (> y (- n 1))
	(let loop-x ((x 0))
	  (unless (> x (- n 1))
	    (set! bitnum (+ 1 bitnum))
	    (set! byteacc (+ (* 2 byteacc) (mandelbrot +iterations+ x y n)))
	    (cond
	     ((= bitnum 8)
	      (write-char (integer->char byteacc) out)
	      (set! bitnum 0)
	      (set! byteacc 0))
	     ((= x (- n 1))
	      (write-char (integer->char (* byteacc (expt 2 (- 8 (modulo n 8))))) out)
	      (set! bitnum 0)
	      (set! byteacc 0)))
	    (loop-x (+ 1 x)) ))
	(loop-y (+ 1 y)) ))
    (with-output-to-file "mandelbrot.ppm"
      (cut display (get-output-string out)))))

(main '("1000"))

))
