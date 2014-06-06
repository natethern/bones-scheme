;; default base

(cond-expand
  (x86_64
   (provide flonums ieee754)
   (files "x86_64/intrinsics.scm")))

(cond-expand
  (linux 
   (provide file-ports time jiffy-clock file-system process-environment))
  (else))

(files "r5rs.scm"
       "r7rs.scm"
       "nonstd.scm")
