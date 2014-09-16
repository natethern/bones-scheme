;;;; system-calls for *BSD (actually library-calls)


(define-syntax-rule (%close fd)
  ($inline "FIX2INT rax; LIBCALL1 close, rax; INT2FIX rax" fd))

(define-syntax-rule (%write buf fd n)
  ($inline "FIX2INT r11; FIX2INT r15; add rax, CELLS(1); LIBCALL3 write, r11, rax, r15; INT2FIX rax" buf fd n))

(define-syntax-rule (%read buf fd n)
  ($inline "FIX2INT r11; FIX2INT r15; add rax, CELLS(1); LIBCALL3 read, r11, rax, r15; INT2FIX rax" buf fd n))

(define-syntax-rule (%open-input-file name)
  ($inline "CALL copy_to_buffer; FIX2INT r11; LIBCALL3 open, buffer, r11, 0; INT2FIX rax" name %O_RDONLY))

(define-syntax-rule (%open-output-file name)
  ($inline "CALL copy_to_buffer; FIX2INT r11; FIX2INT r15; LIBCALL3 open, buffer, r11, r15; INT2FIX rax" 
	   name
	   (%bitwise-ior %O_WRONLY (%bitwise-ior %O_CREAT %O_TRUNC))
	   (%bitwise-ior %S_IRUSR (%bitwise-ior %S_IWUSR (%bitwise-ior %S_IRGRP %S_IROTH)))))

(define-syntax-rule (%open-append-file name)
  ($inline "CALL copy_to_buffer; FIX2INT r11; FIX2INT r15; LIBCALL3 open, buffer, r11, r15; INT2FIX rax" 
	   name
	   (%bitwise-ior %O_WRONLY (%bitwise-ior %O_CREAT %O_APPEND))
	   (%bitwise-ior %S_IRUSR (%bitwise-ior %S_IWUSR (%bitwise-ior %S_IRGRP %S_IROTH)))))

(define-syntax-rule (%time)
  ($inline "LIBCALL1 time, 0; INT2FIX rax"))

(define-syntax-rule (%getenv str)
  ($inline 
   "CALL copy_to_buffer; LIBCALL1 getenv, buffer; test rax, rax; if z; mov rax, FALSE; else; CALL alloc_zstring; endif" 
   str))

(define-syntax-rule (%clock)
  (let ((secs ($inline "FIX2INT rax; LIBCALL2 clock_gettime, rax, buffer; mov rax, [buffer]; INT2FIX rax" %CLOCK_REALTIME)))
    (%fx+ (%fx* secs 1000000000) ($inline "mov rax, [buffer + CELLS(1)]; INT2FIX rax"))))

(define-syntax-rule (%clocks-per-sec)
  (let ((secs ($inline "FIX2INT rax; LIBCALL2 clock_getres, rax, buffer; mov rax, [buffer]; INT2FIX rax" %CLOCK_REALTIME)))
    (%fx+ (%fx* secs 1000000000) ($inline "mov rax, [buffer + CELLS(1)]; INT2FIX rax"))))

(define-syntax-rule (%getcwd)
  ($inline "LIBCALL2 getcwd, buffer, 1024; test rax, rax; if z; mov rax, FALSE; else; CALL alloc_zstring; endif"))

(define-syntax-rule (%chdir dir)
  ($inline "CALL copy_to_buffer; LIBCALL1 chdir, buffer; INT2FIX rax" dir))

(define-syntax-rule (%unlink str)
  ($inline "CALL copy_to_buffer; LIBCALL1 unlink, buffer; INT2FIX rax" str))

(define-syntax-rule (%exists? str)
  ($inline "CALL copy_to_buffer; LIBCALL2 stat, buffer, stat_buffer; test rax, rax; SET_T rax; cmovnz rax, FALSE" str))

(define-syntax-rule (%getpid)
  ($inline "LIBCALL0 getpid; INT2FIX rax"))

(define-syntax-rule (%system cmd)
  ($inline "CALL copy_to_buffer; LIBCALL1 system, buffer; INT2FIX rax" cmd))

(define-syntax-rule (%errno-string)
  ($inline "CALL get_last_error"))

(define-syntax-rule (%exit code)
  ($inline "FIX2INT rax; LIBCALL1 exit, rax" code))

(define-syntax-rule (%_exit code)
  ($inline "FIX2INT rax; LIBCALL1 _exit, rax" code))

(define-syntax-rule (%sigaction num m)
  (begin
    ($inline 
     "test rax, 1; if z; mov rax, signal_handler; else; FIX2INT rax; endif; mov [sigaction_handler], rax"
     (cond ((%eq? m #f) %SIG_IGN)
	   ((%eq? m #t) %SIG_DFL)
	   (else #f)))			; use signal_handler
    ($inline "FIX2INT rax; LIBCALL3 sigaction, rax, sigaction_buf, 0; INT2FIX rax" num)))
