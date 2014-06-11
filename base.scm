;; default base

(cond-expand
  (x86_64
   (files "x86_64/intrinsics.scm"))
  (else (error "no architecture selected")))

(cond-expand
  ((not (or linux windows macosx linux-bare))
   ;; map default-configuration to actual, if no specific target is given
   (cond-expand
     (default-windows (provide windows))
     (default-macosx (provide macosx))
     (default-linux (provide linux))))
  (else))

(cond-expand
  ((or linux linux-bare windows macosx)
   (provide file-ports file-system)
   (cond-expand
     ((or linux windows macosx)
      (provide time jiffy-clock file-system process-environment flonums ieee754))
     (else)))
  (else))

(provide srfi-6)

(files "r5rs.scm"
       "nonstd.scm")
