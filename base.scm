;; default base

(cond-expand
  (x86_64
   (cond-expand 
     ((not bare) (provide flonums ieee754))
     (else))
   (files "x86_64/intrinsics.scm")))

(cond-expand
  (linux 
   (provide file-ports file-system)
   (cond-expand
     ((not bare)
      (provide time jiffy-clock file-system process-environment))
     (else)))
  (else))

(provide srfi-6)

(files "r5rs.scm"
       "nonstd.scm")
