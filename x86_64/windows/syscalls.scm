;;;; system-calls for windows


(define-syntax-rule (%close fd)
  ($inline "FIX2INT rax; LIBCALL1 _close, rax; INT2FIX rax" fd))

(define-syntax-rule (%write buf fd n)
  ($inline "FIX2INT r11; FIX2INT r15; add rax, CELLS(1); LIBCALL3 _write, r11, rax, r15; INT2FIX rax" buf fd n))

(define-syntax-rule (%read buf fd n)
  ($inline "FIX2INT r11; FIX2INT r15; add rax, CELLS(1); LIBCALL3 _read, r11, rax, r15; INT2FIX rax" buf fd n))

(define-syntax-rule (%open name flags mode)
  ($inline "CALL copy_to_buffer; FIX2INT r11; FIX2INT r15; LIBCALL3 _open, buffer, r11, r15; INT2FIX rax" name flags mode))

(define-syntax-rule (%time)
  ($inline "LIBCALL1 time, 0; INT2FIX rax"))

(define-syntax-rule (%getenv str)
  ($inline 
   "CALL copy_to_buffer; LIBCALL1 getenv, buffer; test rax, rax; if z; mov rax, FALSE; endif; CALL alloc_zstring" 
   str))

(define-syntax-rule (%clock)
  ($inline "LIBCALL0 clock; INT2FIX rax"))

(define-syntax-rule (%getcwd)
  ($inline "LIBCALL2 _getcwd, buffer, 1024; mov rax, buffer; CALL alloc_zstring"))

(define-syntax-rule (%chdir dir)
  ($inline "CALL copy_to_buffer; LIBCALL1 _chdir, buffer; INT2FIX rax" dir))

(define-syntax-rule (%unlink str)
  ($inline "CALL copy_to_buffer; LIBCALL1 _unlink, buffer; INT2FIX rax" str))

(define-syntax-rule (%exists? str)
  ($inline "CALL copy_to_buffer; LIBCALL2 _stat, buffer, stat_buffer; test rax, rax; SET_T rax; cmovnz rax, FALSE" str))

(define-syntax-rule (%getpid)
  ($inline "LIBCALL0 getpid; INT2FIX rax"))

(define-syntax-rule (%system cmd)
  ($inline "CALL copy_to_buffer; LIBCALL1 system, buffer; INT2FIX rax" cm))

(define-syntax-rule (%errno-string)
  ($inline "CALL get_last_error"))
