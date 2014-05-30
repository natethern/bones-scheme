;; default base

(files "moresyntax.scm")

(cond-expand
  (x86_64
   (provide flonums ieee754)
   (files "x86_64/intrinsics.scm"))
  (mips
   (files "mips/intrinsics.scm")))

(cond-expand
  (linux 
   (provide file-ports time jiffy-clock file-system process-environment))
  (else))

(files "r5rs.scm"
       "nonstd.scm")
