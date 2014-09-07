;;;; a simple BONES REPL


(program

 (provide check)

 (include "base.scm")
 (include "eval.scm")

 (files "version.scm"
	"pp.scm")

 (code

  (eval-trace #t)
  (repl-print pp)
  (catch-interrupt)

  (let ((bootfile
	 (or (get-environment-variable "SI_BOOT_FILE")
	     (file-exists? "/etc/si-boot.scm"))))
    (when bootfile (load bootfile)))

  (let ((args (cdr (command-line))))
    (cond ((null? args)
	   (cond ((get-environment-variable "HOME") =>
		  (lambda (home)
		    (let ((rcfile (file-exists? (string-append home "/.sirc"))))
		      (when rcfile (load rcfile))))))
	   (print "(BONES " bones-version ")")
	   (repl))
	  (else (load (car args))))) ) )
