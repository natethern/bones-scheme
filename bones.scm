;;;; program-description for self-compile


(program
 (include "base.scm")
 (code
  (define (features) '(bones))
  (define flush-output void))
 (files "version.scm"
	"match.scm"
	"support.scm"
	"pp.scm"
	"alexpand.scm"
	"megalet.scm"
	"source.scm"
	"cp.scm"
	"uve.scm"
	"cc.scm"
	"cps.scm"
	"mangle.scm"
	"program.scm"
	"tsort.scm"
	"cmplr.scm"
	"main.scm")
 (files "x86_64.scm")		; default target
 (code (main (cdr (command-line)))))
