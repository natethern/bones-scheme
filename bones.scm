;;;; program-description for self-compile


(program
 (include "base.scm")
 (code
  (define (features) '(bones))
  (define flush-output void))
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
	"main.scm")
 (cond-expand
   (mips-target (files "mips.scm"))
   (else (files "x86_64.scm")))		; default target
 (code (main (cdr (command-line)))))
