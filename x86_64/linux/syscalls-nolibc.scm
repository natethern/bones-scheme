;;;; system-calls for linux


(define-syntax-rule (%close fd)
  ($inline "FIX2INT rax; SYSCALL1 3, rax; INT2FIX rax" fd))

(define-syntax-rule (%write buf fd n)
  ($inline "FIX2INT r11; FIX2INT r15; add rax, CELLS(1); SYSCALL3 1, r11, rax, r15; INT2FIX rax" buf fd n))

(define-syntax-rule (%read buf fd n)
  ($inline "FIX2INT r11; FIX2INT r15; add rax, CELLS(1); SYSCALL3 0, r11, rax, r15; INT2FIX rax" buf fd n))

(define-syntax-rule (%open name flags mode)
  ($inline "call copy_to_buffer; FIX2INT r11; FIX2INT r15; SYSCALL3 2, buffer, r11, r15; INT2FIX rax" name))

(define-syntax-rule (%getcwd)
  ($inline "SYSCALL2 79, buffer, 1024; mov rax, buffer; call alloc_zstring"))

(define-syntax-rule (%chdir dir)
  ($inline "call copy_to_buffer; SYSCALL1 80, buffer; INT2FIX rax" dir))

(define-syntax-rule (%unlink str)
  ($inline "call copy_to_buffer; SYSCALL1 87, buffer; INT2FIX rax" str))

(define-syntax-rule (%exists? str)
  ($inline "call copy_to_buffer; SYSCALL2 4, buffer, stat_buffer; test rax, rax; SET_T rax; cmovnz rax, FALSE" str))

(define-syntax-rule (%getpid)
  ($inline "SYSCALL0 39; INT2FIX rax"))

(define-syntax-rule (%system cmd)
  (let ((cmd cmd))
    (define (fork) ($inline "SYSCALL0 57; INT2FIX rax"))
    (define (execve)
      ($inline "call copy_to_buffer" cmd)
      ($inline "section .data; system_sh: db '/bin/sh', 0; system_sh_c: db '-c', 0; section .text")
      ($inline "mov r11, stat_buffer; lea rax, [system_sh]; mov [r11 + CELLS(0)], rax; lea rax, [system_sh_c]; mov [r11 + CELLS(1)], rax")
      ($inline "mov r11, stat_buffer; lea rax, [buffer]; mov [r11 + CELLS(2)], rax; xor rax, rax; mov [r11 + CELLS(3)], rax")
      ($inline "SYSCALL3 59, system_sh, stat_buffer, [envp]; INT2FIX rax"))
    (define (waitid pid)
      ($inline "FIX2INT rax; SYSCALL5 247, 0, rax, buffer, 4, 0; INT2FIX rax" pid)) ; WEXITED
    (define (status) 
      ($inline "mov eax, [buffer + 6 * 4]; INT2FIX rax"))
    (let ((pid (fork)))
      (when (eq? pid 0) (execve))		; child process
      (waitid pid)			; parent
      (status))))

(define-syntax-rule (%errno-string) "system call failed")

(define-syntax-rule (%getenv str)
  (let ((str str))
    (let loop ((i 0))
      (let ((var ($inline
		  "FIX2INT rax; mov r11, [envp]; mov rax, [r11 + rax * CELLS(1)]; test rax, rax; if z; mov rax, FALSE; else; call alloc_zstring; endif"
		  i)))
	(and var
	     (let ((len (string-length var)))
	       (let scan ((j 0))
		 (cond ((%fx>=? j len) (loop (%fx+ i 1)))
		       ((char=? #\= (string-ref var j))
			(if (string=? str (substring var 0 j))
			    (substring var (%fx+ j 1))
			    (scan (%fx+ j 1))))
		       (else (scan (%fx+ j 1)))))))))))

(define-syntax-rule (%time)
  ($inline "SYSCALL1 201, 0; INT2FIX rax"))

(define-syntax-rule (%clock)
  (let ((secs ($inline "SYSCALL2 227, 1, buffer; mov rax, [buffer]; INT2FIX rax"))) ; CLOCK_MONOTONIC
    (%fx+ (%fx* secs 1000000000) ($inline "mov rax, [buffer + CELLS(1)]; INT2FIX rax"))))

(define %clocks-per-sec
  (let ((secs ($inline "SYSCALL2 229, 1, buffer; mov rax, [buffer]; INT2FIX rax"))) ; CLOCK_MONOTONIC
    (%fx+ (%fx* secs 1000000000) ($inline "mov rax, [buffer + CELLS(1)]; INT2FIX rax"))))
