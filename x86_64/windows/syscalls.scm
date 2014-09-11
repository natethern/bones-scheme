;;;; system-calls for Windows (actually library-calls)


(define-syntax-rule (%close fd)
  ($inline "FIX2INT rax; LIBCALL1 _close, rax; INT2FIX rax" fd))

(define-syntax-rule (%write buf fd n)
  ($inline "FIX2INT r11; FIX2INT r15; add rax, CELLS(1); LIBCALL3 _write, r11, rax, r15; cdqe; INT2FIX rax" buf fd n))

(define-syntax-rule (%read buf fd n)
  ($inline "FIX2INT r11; FIX2INT r15; add rax, CELLS(1); LIBCALL3 _read, r11, rax, r15; cdqe; INT2FIX rax" buf fd n))

(define-syntax-rule (%open-input-file name)
  ($inline "CALL copy_to_buffer; FIX2INT r11; LIBCALL3 _open, buffer, r11, 0; cdqe; INT2FIX rax"
	   name
	   (%bitwise-ior %O_RDONLY %O_BINARY)))

(define-syntax-rule (%open-output-file name)
  ($inline "CALL copy_to_buffer; FIX2INT r11; FIX2INT r15; LIBCALL3 _open, buffer, r11, r15; cdqe; INT2FIX rax" 
	   name
	   (%bitwise-ior %O_WRONLY (%bitwise-ior %O_CREAT (%bitwise-ior %O_TRUNC %O_BINARY)))
	   (%bitwise-ior %_S_IREAD %_S_IWRITE)))

(define-syntax-rule (%open-append-file name)
  ($inline "CALL copy_to_buffer; FIX2INT r11; FIX2INT r15; LIBCALL3 _open, buffer, r11, r15; cdqe; INT2FIX rax" 
	   name
	   (%bitwise-ior %O_WRONLY (%bitwise-ior %O_CREAT (%bitwise-ior %O_APPEND %O_BINARY)))
	   (%bitwise-ior %_S_IREAD %_S_IWRITE)))

(define-syntax-rule (%time)
  ($inline "LIBCALL1 time, 0; INT2FIX rax"))

(define-syntax-rule (%getenv str)
  ($inline 
   "CALL copy_to_buffer; LIBCALL1 getenv, buffer; test rax, rax; if z; mov rax, FALSE; else; CALL alloc_zstring; endif" 
   str))

(define-syntax-rule (%clock)
  ($inline "push rax; mov r11, rsp; LIBCALL1 QueryPerformanceCounter, r11; pop rax; INT2FIX rax"))

(define-syntax-rule (%clocks-per-sec)
  ($inline "push rax; mov r11, rsp; LIBCALL1 QueryPerformanceFrequency, r11; pop rax; INT2FIX rax"))

(define-syntax-rule (%getcwd)
  ($inline "LIBCALL2 _getcwd, buffer, 1024; test rax, rax; if z; mov rax, FALSE; else; mov rax, buffer; CALL alloc_zstring; endif"))

(define-syntax-rule (%chdir dir)
  ($inline "CALL copy_to_buffer; LIBCALL1 _chdir, buffer; cdqe; INT2FIX rax" dir))

(define-syntax-rule (%unlink str)
  ($inline "CALL copy_to_buffer; LIBCALL1 _unlink, buffer; cdqe; INT2FIX rax" str))

(define-syntax-rule (%exists? str)
  ($inline "CALL copy_to_buffer; LIBCALL2 _stat, buffer, stat_buffer; cdqe; test rax, rax; SET_T rax; cmovnz rax, FALSE" str))

(define-syntax-rule (%getpid)
  ($inline "LIBCALL0 _getpid; cdqe; INT2FIX rax"))

(define-syntax-rule (%system cmd)
  ($inline "CALL copy_to_buffer; LIBCALL1 system, buffer; cdqe; INT2FIX rax" cmd))

(define-syntax-rule (%errno-string)
  ($inline "CALL get_last_error"))

(define-syntax-rule (%exit code)
  ($inline "FIX2INT rax; LIBCALL1 exit, rax" code))

(define-syntax-rule (%_exit code)
  ($inline "FIX2INT rax; LIBCALL1 _exit, rax" code))

(define-syntax-rule (%sigaction num m)
  ($inline 
   "FIX2INT rax; test r11, 1; if nz; mov rax, signal_handler; endif; LIBCALL2 signal, rax, r11"
   num
   (cond ((%eq? m #f) %SIG_IGN)
	 ((%eq? m #t) %SIG_DFL)
	 (else #f))))			; use signal_handler
