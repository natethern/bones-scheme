;;;; program-description for self-compile


(program
 (include "base.scm")
 (code
  (define (features) '(bones))
  (define flush-output void)
  (define (command-line-arguments) (cdr (command-line))))
 (files "match.scm"
	"support.scm"
	"pp.scm"
	"alexpand.scm"
	"megalet.scm"
	"source.scm"
	"cc.scm"
	"cps.scm"
	"mangle.scm"
	"program.scm"
	"cmplr.scm"
	"x86_64.scm")
 (code (main (command-line-arguments))))
