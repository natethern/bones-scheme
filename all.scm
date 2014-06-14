;;;; loader


(require 'syntax 'match 'stuff 'megalet 'logical)

(features
 (cons (case (system-software)
	 ((Linux) 'linux)
	 (else (error "unknown host system")))
       (features)))

(load "alexpand.scm")
(load "source.scm")
(load "cc.scm")
(load "cps.scm")
(load "mangle.scm")
(load "program.scm")
(load "cmplr.scm")
(load "x86_64.scm")
(load "main.scm")

(load "repl.scm")
