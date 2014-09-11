;; default base configurations


 ;; convenience syntax, also used in the rest of the library
(code
 (define-syntax define-syntax-rule
   (syntax-rules
       ___ ()
       ((_ (name args ___) rule)
	(define-syntax name
	  (syntax-rules ()
	    ((_ args ___) rule)))))))

;; intrinsics - needed for standard and non-standard procedures
(cond-expand
  (x86_64
   (provide ieee754)
   (files "x86_64/intrinsics.scm"))
  (else (error "no architecture selected")))

;; select default target, if none is given on the command line
(cond-expand
  ((not (or linux windows mac bsd))
   ;; map default-configuration to actual, if no specific target is given
   (cond-expand
     (default-linux (provide linux))
     (default-bsd (provide bsd))
     (default-windows (provide windows))
     (default-mac (provide mac))))
  (else))

;; include OS-specific definitions and features
(cond-expand
  (linux
   (provide file-ports file-system process-environment time jiffy-clock lp64)
   (files "x86_64/linux/constants.scm")
   (cond-expand
     (x86_64
      (cond-expand
	(nolibc (files "x86_64/linux/syscalls-nolibc.scm"))
	(else (files "x86_64/linux/syscalls.scm"))))
     (else (error "unsupported architecture for linux"))))
  (bsd
   (provide file-ports file-system process-environment time jiffy-clock lp64)
   (files "x86_64/bsd/constants.scm")
   (cond-expand
     (x86_64 (files "x86_64/bsd/syscalls.scm"))
     (else (error "unsupported architecture for bsd"))))
  (mac
   (provide file-ports file-system process-environment time jiffy-clock lp64 pic)
   (files "x86_64/mac/constants.scm")
   (cond-expand
     (x86_64 (files "x86_64/mac/syscalls.scm"))
     (else (error "unsupported architecture for mac"))))
  (windows
   (provide file-ports file-system time jiffy-clock file-system
	    process-environment pic llp64)
   (files "x86_64/windows/constants.scm"
	  "x86_64/windows/syscalls.scm"))
  (else (error "no operating system selected")))

;; some SRFI-features that are provided via libraries
(provide srfi-6)

;; add primitives
(files "r5rs.scm"
       "nonstd.scm")
