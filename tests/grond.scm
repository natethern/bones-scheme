;;;; run grond on bones


(program
 (include "base.scm")
 (files "pp.scm")
 (code

  (define-syntax define-unimplemented
    (syntax-rules ()
      ((_ name) 
       (define (name . _) (error 'name "unimplemented")))))

  (define-unimplemented repl)
  (define-unimplemented eval)
  (define-unimplemented oblist)
  (define-unimplemented current-source-filename)
  (define-unimplemented interaction-environment)
  (define-unimplemented load)

  (define *loaded-modules* '())
  (define (grass-version) #f)
  (define (implementation-name) "bones")
  (define (library-path . path) '())
  (define (features) '())
  (define current-time current-second)
  (define flush-output void)

  (define (command-line-arguments) (cdr (command-line)))

  (define call-with-exit-continuation call/cc)
  (define compile-error error) )

 (files "tests/grond-base.scm")
 (code (main (command-line-arguments))))
