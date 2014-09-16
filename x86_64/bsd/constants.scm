;;; BSD
(define-syntax %SIGINT 2)
(define-syntax %SIG_IGN 1)
(define-syntax %SIG_DFL 0)
(define-syntax %O_RDONLY 0)
(define-syntax %O_WRONLY 1)
(define-syntax %O_CREAT 512)
(define-syntax %O_TRUNC 1024)
(define-syntax %O_APPEND 8)
(define-syntax %S_IRUSR 256)
(define-syntax %S_IWUSR 128)
(define-syntax %S_IRGRP 32)
(define-syntax %S_IWGRP 16)
(define-syntax %S_IROTH 4)
(define-syntax %CLOCK_REALTIME 0)	; just guessing
;; sigaction-size: 16
;; sigaction-handler-offset: 0
