;; default base

(cond-expand
  (x86_64
   (files "x86_64/intrinsics.scm"))
  (else (error "no architecture selected")))

(cond-expand
  ((not (or linux linux-bare))
   ;; map default-configuration to actual, if no specific target is given
   (cond-expand
     (default-linux (provide linux))))
  (else))

(cond-expand
  ((or linux linux-bare)
   (provide file-ports file-system)
   (cond-expand
     (linux
      (provide time jiffy-clock file-system process-environment flonums ieee754))
     (else)))
  (else))

(provide srfi-6)

(files "r5rs.scm"
       "nonstd.scm")
