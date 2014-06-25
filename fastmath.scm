;;;; include-file for fastmath


(cond-expand
  (x86_64
   (provide fastmath)
   (files "x86_64/fastmath.scm"))
  (else
   (error "Sorry, fastmath.scm is not available for this architecture")))
