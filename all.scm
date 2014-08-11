;;;; loader


(require 'syntax 'match 'stuff 'megalet 'logical)

(features
 (cons (case (system-software)
	 ((Linux) 'linux)
	 (else (error "unknown host system")))
       (features)))

(define bytevector? (const #f))
(define (bytevector-length x) (error "no bytevectors"))
(define (bytevector-u8-ref x y) (error "no bytevectors"))
(define (nan? x) (and (number? x) (not (= x x))))
(define (finite? x) (not (= (string->number "+inf.0") (abs x))))

(load "version.scm")
(load "alexpand.scm")
(load "source.scm")
(load "cp.scm")
(load "simplify.scm")
(load "uv.scm")
(load "cc.scm")
(load "cps.scm")
(load "mangle.scm")
(load "program.scm")
(load "tsort.scm")
(load "ra.scm")
(load "cmplr.scm")
(load "x86_64.scm")
(load "main.scm")

(load "repl.scm")
