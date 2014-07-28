;;;; a simple BONES REPL


(program

 (include "base.scm")
 (include "eval.scm")

 (files "version.scm")

 (code

  (eval-trace #t)

  (let ((args (cdr (command-line))))
    (cond ((null? args)
	   (print "(BONES " bones-version ")")
	   (repl))
	  (else (load (car args))))) ) )
