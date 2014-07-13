;;;; a simple BONES REPL


(program

 (include "base.scm")
 (include "eval.scm")

 (code
  (do () (#f)
    (display "> ")
    (let ((x (read)))
      (when (eof-object? x) (exit))
      (call/cc
       (lambda (return)
	 (parameterize ((current-exception-handler
			 (lambda (exn)
			   (let ((out (current-error-port)))
			     (newline out)
			     (cond ((error-object? exn)
				    (display "Error: " out)
				    (display (error-object-message exn) out)
				    (for-each
				     (lambda (x)
				       (newline out)
				       (write x out)
				       (newline out))
				     (error-object-irritants exn)))
				   (else
				    (display "Unhandled excception: " out)
				    (write exn out)
				    (newline out)))
			     (return #f)))))
	   (write (eval x))
	   (newline)))) ))) )
