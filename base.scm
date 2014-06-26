;; default base configurations


;; intrinsics - needed for standard and non-standard procedures
(cond-expand
  (x86_64
   (provide ieee754)
   (files "x86_64/intrinsics.scm"))
  (else (error "no architecture selected")))

;; select default target, if none is given on the command line
(cond-expand
  ((not (or linux windows))
   ;; map default-configuration to actual, if no specific target is given
   (cond-expand
     (default-windows (provide windows))
     (default-linux (provide linux))))
  (else))

;; include OS-specific definitions and features
(cond-expand
  (linux
   (provide file-ports file-system process-environment time jiffy-clock
	    lp64)
   (cond-expand
     (x86_64
      (cond-expand
	(nolibc (files "x86_64/linux/syscalls-nolibc.scm"))
	(else (files "x86_64/linux/syscalls.scm"))))
     (else (error "unsupported architecture for linux"))))
  (windows
   (provide file-ports file-system time jiffy-clock file-system
	    process-environment pic llp64)
   (files "x86_64/windows/syscalls.scm"))
  (else (error "no operating system selected")))

;; some SRFI-features that are always available
(provide srfi-6 srfi-16)

;; add primitives
(files "r5rs.scm"
       "nonstd.scm")
