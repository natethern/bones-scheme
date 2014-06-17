;;;; system-calls for linux


(define-syntax-rule (%close fd)
  ($inline "FIX2INT rax; SYSCALL1 3, rax; INT2FIX rax" fd))

(define-syntax-rule (%write buf fd n)
  ($inline "FIX2INT r11; FIX2INT r15; add rax, CELLS(1); SYSCALL3 1, r11, rax, r15; INT2FIX rax" buf fd n))

(define-syntax-rule (%read buf fd n)
  ($inline "FIX2INT r11; FIX2INT r15; add rax, CELLS(1); SYSCALL3 0, r11, rax, r15; INT2FIX rax" buf fd n))

(define-syntax-rule (%open name flags mode)
  ($inline "CALL copy_to_buffer; FIX2INT r11; FIX2INT r15; SYSCALL3 2, buffer, r11, r15; INT2FIX rax" name))

(define-syntax-rule (%getcwd)
  ($inline "SYSCALL2 79, buffer, 1024; mov rax, buffer; CALL alloc_zstring"))

(define-syntax-rule (%chdir dir)
  ($inline "CALL copy_to_buffer; SYSCALL1 80, buffer; INT2FIX rax" dir))

(define-syntax-rule (%unlink str)
  ($inline "CALL copy_to_buffer; SYSCALL1 87, buffer; INT2FIX rax" str))

(define-syntax-rule (%exists? str)
  ($inline "CALL copy_to_buffer; SYSCALL2 4, buffer, stat_buffer; test rax, rax; SET_T rax; cmovnz rax, FALSE" str))

(define-syntax-rule (%getpid)
  ($inline "SYSCALL0 39; INT2FIX rax"))

(define-syntax-rule (%system cmd)
  (%error '%system "not implemented"))

(define-syntax-rule (%errno) "system call failed")

(define-syntax-rule (%getenv cmd)
  (%error '%getenv "not implemented"))
