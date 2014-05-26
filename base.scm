;; default base

(files "moresyntax.scm")

(cond-expand
  (x86_64
   (files "x86_64/intrinsics.scm")))

(files "r5rs.scm"
       "nonstd.scm")
