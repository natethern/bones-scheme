;;;; libc stubs


section .text


extern exit
extern write
extern read
extern system
extern time
extern getpid
extern getenv
extern unlink
extern stat
extern open
extern close
extern strtol
extern strtod
extern sprintf
extern getcwd
extern chdir
extern clock


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; x86-64 ABI requires rsp to be aligned on a 16-byte boundary, sets rax to 0
%macro ALIGN_STACK 0
  mov qword [rsp_save], rsp
  mov rax, ~15
  and rsp, rax
  xor rax, rax
%endmacro

%define RESTORE_STACK mov rsp, qword [rsp_save]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; write error message and exit: rax = raw string, r11 = length
write_error_and_exit:
  mov rdi, 2			; stderr
  mov rsi, rax
  mov rdx, r11
  ALIGN_STACK
  call write			; no need to restore stack here
  mov rdi, 70			; EXIT_FAILURE
  call exit


;; format string using sprintf(3) and write to stderr: rax = raw format-string, r11, r15 = args
format_string:
  SAVE
  mov rdi, buffer
  mov rsi, rax
  mov rdx, r11
  mov rcx, r15
  ALIGN_STACK
  call sprintf
  mov rdi, 2
  mov rsi, buffer
  mov rdx, rax
  call write
  RESTORE_STACK
  RESTORE
  ret  


;; allocate 0-terminated string: rax = charbuffer-ptr -> rax (string)
;; does not check the heap-limit, so is only usable for small strings.
alloc_zstring:
  push rdi
  push rsi
  mov rsi, rax
  lea rdi, [ALLOC + CELLS(1)]
  repeat
    lodsb
    test al, al
  while nz
    stosb
  again
  mov rax, rdi
  sub rax, ALLOC
  sub rax, CELLS(1)
  mov rsi, STRING
  or rax, rsi
  mov [ALLOC], rax
  add rdi, 7			; align
  mov rsi, ~7
  and rdi, rsi
  mov rax, ALLOC
  mov ALLOC, rdi
  pop rsi
  pop rdi
  ret


;; copy string into buffer and adds 0-terminator: rax = string
copy_to_buffer:
  push rdi
  push rsi
  push rcx
  mov rcx, [rax]
  mov rdi, SIZE_MASK
  and rcx, rdi
  lea rsi, [rax + CELLS(1)]
  mov rdi, buffer
  rep movsb
  xor rcx, rcx
  mov [rdi], cl
  pop rcx
  pop rsi
  pop rdi
  ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; syscall_exit: rax = code
syscall_exit:
  FIX2INT rax
  mov rdi, rax
  ALIGN_STACK
  call exit


;; syscall_write: rax = string, r11 = fd, r15 = count -> rax (returncode)
syscall_write:
  SAVE
  mov rdi, r11
  FIX2INT rdi		; fd
  lea rsi, [rax + 8]	; buffer
  FIX2INT r15
  mov rdx, r15    	; count
  ALIGN_STACK
  call write
  INT2FIX rax
  RESTORE_STACK
  RESTORE
  ret


;; syscall_read: rax = string, r11 = fd, r15 = bytes -> rax (returncode)
syscall_read:
  SAVE
  mov rdi, r11
  FIX2INT rdi		; fd
  lea rsi, [rax + 8]	; buffer
  mov rdx, 1	    	; count
  ALIGN_STACK
  call read
  INT2FIX rax
  RESTORE_STACK
  RESTORE
  ret


;; open input fd: rax = string -> rax (fd)
syscall_open_input:
  SAVE
  call copy_to_buffer
  mov rdi, buffer
  mov rsi, 0			; flags: O_RDONLY
  mov rdx, 0o700		; mode: S_IRWXU
  ALIGN_STACK
  call open
  INT2FIX rax
  RESTORE_STACK
  RESTORE
  ret


;; open output fd: rax = string -> rax (fd)
syscall_open_output:
  SAVE
  call copy_to_buffer
  mov rdi, buffer
  mov rsi, 577			; flags: O_WRONLY|O_CREAT|O_TRUNC
  mov rdx, 0o700		; mode: S_IRWXU
  ALIGN_STACK
  call open
  INT2FIX rax
  RESTORE_STACK
  RESTORE
  ret


;; open append output fd: rax = string -> rax (fd)
syscall_open_append:
  SAVE
  call copy_to_buffer
  mov rdi, buffer
  mov rsi, 1089			; flags: O_WRONLY|O_CREAT|O_APPEND
  mov rdx, 0o700		; S_IRWXU
  ALIGN_STACK
  call open
  INT2FIX rax
  RESTORE_STACK
  RESTORE
  ret


;; close fd: rax = fd -> rax (returncode)
syscall_close:
  SAVE
  FIX2INT rax
  ALIGN_STACK
  call close
  INT2FIX rax
  RESTORE_STACK
  RESTORE
  ret


;; syscall_shell_command: rax = str -> rax (code)
syscall_shell_command:
  SAVE
  call copy_to_buffer
  mov rdi, buffer
  ALIGN_STACK
  call system
  RESTORE_STACK
  RESTORE
  INT2FIX rax
  ret


;; compute current time: -> rax (seconds)
syscall_time:
  SAVE
  xor rdi, rdi
  ALIGN_STACK
  call time
  RESTORE_STACK
  RESTORE
  ret


;; get current process id: -> rax (pid)
syscall_getpid:
  SAVE
  ALIGN_STACK
  call getpid
  RESTORE_STACK
  RESTORE
  ret


;; get environment variable: rax = string -> rax = string or #f
syscall_getenv:
  SAVE
  call copy_to_buffer
  mov rdi, buffer
  ALIGN_STACK
  call getenv
  ;; doesn't check for heap overflow
  RESTORE_STACK
  RESTORE
  test rax, rax
  if z
    mov rax, FALSE
    ret
  endif
  call alloc_zstring
  ret


;; delete file: rax = string -> rax (return code)
syscall_delete_file:
  SAVE
  call copy_to_buffer
  mov rdi, buffer
  ALIGN_STACK
  call unlink
  RESTORE_STACK
  RESTORE
  INT2FIX rax
  ret


;; check whether file exists: rax = string -> rax (bool)
syscall_file_exists:
  SAVE
  call copy_to_buffer
  mov rdi, buffer
  mov rsi, stat_buffer
  ALIGN_STACK
  call stat
  test rax, rax
  SET_T rax
  cmovnz rax, FALSE
  RESTORE_STACK
  RESTORE
  ret


;; convert string to number: rax = string, r11 = base -> rax (number)
syscall_str2num:
  SAVE
  call copy_to_buffer
  ALIGN_STACK
  push rax			; endptr
  mov rdi, buffer
  mov rsi, rsp
  FIX2INT r11
  mov rdx, r11
  call strtol
  ;; check endptr being identical to startptr
  pop r11
  mov r15, buffer
  cmp r11, buffer
  if e
    mov rax, FALSE
    jmp .done
  endif
  ;; check endptr for being '\0'
  mov bl, [r11]
  test bl, bl
  if z
    ;; it is an integer
    INT2FIX rax
  else
    ;; now try if it is a float
    mov rdi, buffer
    push rax			; endptr
    mov rsi, rsp
    call strtod			; ignores base
    pop r11
    mov bl, [r11]
    test bl, bl
    if z
      mov rax, FLONUM | 1
      mov [ALLOC], rax
      movsd [ALLOC + CELLS(1)], xmm0
      mov rax, ALLOC
      add ALLOC, CELLS(2)
    else
      mov rax, FALSE
    endif
  endif
.done:
  RESTORE_STACK
  RESTORE
  ret


;; convert number to string: rax = number, r11 = base -> rax (string)
syscall_num2str:
  SAVE
  FIX2INT r11
  mov rdi, stat_buffer
  test rax, 1
  if nz
    cmp r11, 8
    if e
      mov rsi, ocvt
    else
      cmp r11, 16
      if e
        mov rsi, xcvt
      else
        mov rsi, dcvt
      endif
    endif
    mov rdx, rax
    FIX2INT rdx
    ALIGN_STACK
    call sprintf
  else
    mov rsi, gcvt
    movsd xmm0, [rax + CELLS(1)]
    ALIGN_STACK
    mov rax, 1			; 1 float argument
    call sprintf
  endif
  RESTORE_STACK
  mov rax, stat_buffer
  call alloc_zstring
  RESTORE
  ret    


;; get current dir: -> rax (string)
syscall_getcwd:
  SAVE
  mov rdi, stat_buffer
  mov rsi, 1024
  ALIGN_STACK
  call getcwd
  RESTORE_STACK
  mov rax, stat_buffer
  call alloc_zstring
  RESTORE
  ret


;; set current dir: rax = dir -> rax (error code)
syscall_chdir:
  SAVE
  call copy_to_buffer
  mov rdi, buffer
  ALIGN_STACK
  call chdir
  RESTORE_STACK
  INT2FIX rax
  RESTORE
  ret


;; get clock-ticks: -> rax (fixnum)
syscall_clock:
  SAVE
  ALIGN_STACK
  call clock
  RESTORE_STACK
  RESTORE
  INT2FIX rax
  ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


section .data

dcvt: db "%ld", 0
ocvt: db "%lo", 0
xcvt: db "%lx", 0
gcvt: db "%.15g", 0


section .bss

rsp_save: resq 1
stat_buffer: resb 1024
