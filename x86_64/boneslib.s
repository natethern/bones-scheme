;;;; bones.s - runtime system and core library (x86_64) -*- nasm -*-
;
;
; * Register usage: 
;
;	rax, r11, r15: temporary
;	rbx: current closure
;       rbp: allocation-pointer
;       r13: allocation-limit (fromspace-top - reserve area)
;       r14: false
;
; * Data layout:
;
;	BLOCK = [header, slot1, ...]
;       HEADER = [mark-bit (1), type-bits (7), length (56)]
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


%ifndef BONESLIB_S
%define BONESLIB_S


  bits 64


%ifdef FEATURE_PIC
  default rel
 %ifdef FEATURE_LINUX
  %define WRTPLT  wrt ..plt
 %else 
  %define WRTPLT
 %endif
%else
 %define WRTPLT
%endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


%ifndef TOTAL_HEAP_SIZE
 %define TOTAL_HEAP_SIZE 100_000_000
%endif

%define FROMSPACE_RESERVE (TOTAL_HEAP_SIZE / 10)
%define MARK_BIT	0x8000000000000000
%define BITS_MASK       0xff00000000000000
%define SIZE_MASK       0x00ffffffffffffff
%define BYTEBLOCK_BIT   0x1000000000000000
%define SPECIAL_BIT     0x2000000000000000
%define FIX(n)		(((n) << 1) | 1)
%define UNFIX(n)	((n) >> 1)
%define CELLS(n)        ((n) * 8)
%define HEADER_SHIFT    56
%define CELL_SHIFT      3
%define ALIGN_BASE      7
%define WORD_SIZE       8

%define ALIGNED(n)      (((n) + ALIGN_BASE) & ~ALIGN_BASE)
%define TYPECODE(tn)    ((tn) << HEADER_SHIFT)
%define TYPENUMBER(tc)  (((tc) >> HEADER_SHIFT) & 0x0f)

%define SELF		rbx
%define ALLOC		rbp
%define FALSE           r14
%define LIMIT           r13

%define NUMBER_OF_NON_REGISTER_ARGUMENTS 1024
%define NUMBER_OF_ARGUMENT_REGISTERS 9
%define MAXIMUM_NUMBER_OF_ARGUMENTS (NUMBER_OF_NON_REGISTER_ARGUMENTS + NUMBER_OF_ARGUMENT_REGISTERS)

%define NULL    TYPECODE(0)
%define SYMBOL	TYPECODE(1)
%define PAIR	TYPECODE(2)
%define VECTOR	TYPECODE(3)
%define CHAR	TYPECODE(4)
%define EOF	TYPECODE(5)
%define VOID	TYPECODE(6)
%define BOOLEAN TYPECODE(7)
%define PORT    TYPECODE(8)
%define PROMISE  TYPECODE(9)
%define RECORD  TYPECODE(10)
;; byteblock objects
%define FLONUM	TYPECODE(0x10)
%define STRING	TYPECODE(0x11)
%define BYTEVECTOR TYPECODE(0x12)
;; special object
%define CLOSURE	TYPECODE(0x20)
;; pseudo type
%define FIXNUM  TYPECODE(11)


%ifdef FEATURE_CHECK
 %define ENABLE_WRITE_BARRIER
%endif


;; get type-number from value pointed to by %1
%define TYPENUMBER_REF(x) [x + CELLS(1) - 1]

%ifdef FEATURE_EMBEDDED
 %ifdef PREFIX
  %define ENTRYPOINT PREFIX %+ _scheme
 %else
  %define ENTRYPOINT scheme
 %endif
%endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "structured" programming, taken mostly from the NASM documentation

;; if CC
;;   ...
;; [else
;;   ...]
;; endif

%macro if 1
  %push if
  j%-1 %$if_not
%endmacro

%macro else 0
  %ifctx if
    %repl else
    jmp %$if_end
    %$if_not:
  %else
    %error "expected `if' before `else'"
  %endif
%endmacro

%macro endif 0
  %ifctx if
    %$if_not:
    %pop
  %elifctx else
    %$if_end:
    %pop
  %else
    %error "expected `if' or `else' before `endif'"
  %endif
%endmacro


;; repeat
;;   ...
;; [while CC
;;   ...]
;; until CC [or: again]

%macro repeat 0
  %push repeat
  %$begin:
%endmacro

%macro until 1
  %ifctx repeat
    j%-1 %$begin
    %pop
  %elifctx while
    j%-1 %$begin
    %$while_end:
    %pop
  %else
    %error "`until' without `repeat'"
  %endif
%endmacro

%macro while 1
  %ifctx repeat
    j%-1 %$while_end
    %repl while
  %elifctx while
    j%-1 %$while_end
  %else
    %error "`while' without `repeat'"
  %endif
%endmacro

%macro again 0
  %ifctx repeat
    jmp %$begin
    %pop
  %elifctx while
    jmp %$begin
    %$while_end:
    %pop
  %else
    %error "`while' without `repeat'"
  %endif
%endmacro


;; for COUNTER, START, END, [STEP = 1]
;;   ...
;; next

%macro for 3-4 1
  %push for
  mov %1, %2
  %define %$for_counter %1
  %define %$for_limit %3
  %define %$for_step %4
  %$for_loop:
%endmacro

%macro next 0
  %ifctx for
    add %$for_counter, %$for_step
    cmp %$for_counter, %$for_limit
    jne %$for_loop
    %$for_end:
  %else
    %error "`next' without `for`"
  %endif
%endmacro


;; break (exits any loop construct)

%macro break 0
  %ifctx repeat
    %repl while
    jmp %$while_end
  %elifctx while
    jmp %$while_end
  %elifctx for
    jmp %$for_end
  %else
    %error "`break' outside of loop"
  %endif
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; crash
%macro CRASH 0
  xor rax, rax
  jmp rax
%endmacro


%macro CALL 1
  call %1
%endmacro


;; save registers before C function call
%macro SAVE 0
  sub rsp, CELLS(8)
  mov [rsp + CELLS(0)], rbx
  mov [rsp + CELLS(1)], rcx
  mov [rsp + CELLS(2)], rdx
  mov [rsp + CELLS(3)], rsi
  mov [rsp + CELLS(4)], rdi
  mov [rsp + CELLS(5)], r8
  mov [rsp + CELLS(6)], r9
  mov [rsp + CELLS(7)], r10
%endmacro


;; restore registers after C function call
%macro RESTORE 0
  mov r10, [rsp + CELLS(7)]
  mov r9, [rsp + CELLS(6)]
  mov r8, [rsp + CELLS(5)]
  mov rdi, [rsp + CELLS(4)]
  mov rsi, [rsp + CELLS(3)]
  mov rdx, [rsp + CELLS(2)]
  mov rcx, [rsp + CELLS(1)]
  mov rbx, [rsp + CELLS(0)]
  add rsp, CELLS(8)
%endmacro


;; convert fixnum to 64-bit int
%macro FIX2INT 1
  sar %1, 1
%endmacro


;; convert 64-bit int to fixnum
%macro INT2FIX 1
  shl %1, 1
  or %1, 1
%endmacro 


;; set destination-register to TRUE
%macro SET_T 1
  lea %1, [FALSE + CELLS(2)]
%endmacro


;; call continuation: rcx = k, %1 = result
%macro CONTINUE 1
  mov SELF, rcx
  mov rcx, %1
  mov rax, [rbx + CELLS(1)]	; get function-ptr
  mov r11, 2	  	; 2 args (closure + result)
  jmp rax
%endmacro


;; allocate flonum: %1 = flonum -> rax
%macro ALLOC_FLONUM 1
  ;; ALIGNMENT: on 32-bit systems, align so that actual float is on an 8-byte boundary
  movsd [ALLOC + CELLS(1)], %1
  mov rax, FLONUM | CELLS(1)
  mov [ALLOC], rax
  mov rax, ALLOC
  add ALLOC, CELLS(2)
%endmacro


;; continue with a float-result: rcx = k, %1 = float (xmm)
%macro CONTINUE_FLOAT 1
  ALLOC_FLONUM %1
  CONTINUE rax
%endmacro


;; write barrier: %1 = destination, %2 = value (may not be rax), clobbers rax
%macro WRITE_BARRIER 2
%ifdef ENABLE_WRITE_BARRIER
  push r11
  lea rax, %1
  mov r11, %2
  cmp [fromspace], rax
  jae %%evict
  cmp [fromspace_end], rax
  ja %%ok
%%evict:
  call write_barrier_trap
%%ok:
  mov [rax], r11
  pop r11
%else
  lea rax, %1
  mov [rax], %2
%endif
%endmacro

	      
;; define primitive procedure
%macro PRIMITIVE 1
  align 8
%1: 
%endmacro


;; abort with error message: %1 = string
%macro HALT 1
  section .data
%%1: db %1
%%2:
  section .text
  mov rax, %%1
  mov r11, %%2 - %%1
  jmp write_error_and_exit
%endmacro


;; the x86-64 ABI requires rsp to be aligned on a 16-byte boundary, sets rax to 0
%macro ALIGN_STACK 0
  mov qword [rsp_save], rsp
  and rsp, [rsp_alignment_mask]
%endmacro

%define RESTORE_STACK mov rsp, qword [rsp_save]


;; Library-specific name mangling
%define UNDERSCORE(name)      _ %+ name

%ifdef FEATURE_MAC
 %define MANGLE(name)         UNDERSCORE(name)
%else
 %define MANGLE(name)         name
%endif


;; call C function with 0-4 arguments
%macro LIBCALL0 1
  extern MANGLE(%1)
  SAVE
  ALIGN_STACK
%ifdef FEATURE_WINDOWS
  sub rsp, 32
%else
  xor rax, rax
%endif
  call MANGLE(%1) WRTPLT
  RESTORE_STACK
  RESTORE
%endmacro

%macro LIBCALL1 2
  extern MANGLE(%1)
  SAVE
%ifdef FEATURE_WINDOWS
  mov rcx, %2
%else
  mov rdi, %2
%endif
  ALIGN_STACK
%ifdef FEATURE_WINDOWS
  sub rsp, 32
%else
  xor rax, rax
%endif
  call MANGLE(%1) WRTPLT
  RESTORE_STACK
  RESTORE
%endmacro

%macro LIBCALL2 3
  extern MANGLE(%1)
  SAVE
%ifdef FEATURE_WINDOWS
  mov rcx, %2
  mov rdx, %3
%else
  mov rdi, %2
  mov rsi, %3
%endif
  ALIGN_STACK
%ifdef FEATURE_WINDOWS
  sub rsp, 32
%else
  xor rax, rax
%endif
  call MANGLE(%1) WRTPLT
  RESTORE_STACK
  RESTORE
%endmacro

;; library-call with 2 float argument
%macro LIBCALL2_f 3
  extern MANGLE(%1)
  SAVE
%ifdef FEATURE_WINDOWS
  mov rcx, %2
  movq xmm0, rcx		; silly
  mov rdx, %3
  movq xmm1, rdx
%else
  movsd xmm0, %2
  movsd xmm1, %3
%endif
  ALIGN_STACK
%ifdef FEATURE_WINDOWS
  sub rsp, 32
%else
  mov rax, 2
%endif
  call MANGLE(%1) WRTPLT
  RESTORE_STACK
  RESTORE
%endmacro

%macro LIBCALL3 4
  extern MANGLE(%1)
  SAVE
%ifdef FEATURE_WINDOWS
  mov rcx, %2
  mov rdx, %3
  mov r8, %4
%else
  mov rdi, %2
  mov rsi, %3
  mov rdx, %4
%endif
  ALIGN_STACK
%ifdef FEATURE_WINDOWS
  sub rsp, 32
%else
  xor rax, rax
%endif
  call MANGLE(%1) WRTPLT
  RESTORE_STACK
  RESTORE
%endmacro

;; library-call with 3 integer and one float argument
%macro LIBCALL3_1 4
  extern MANGLE(%1)
  SAVE
%ifdef FEATURE_WINDOWS
  mov rcx, %2
  mov rdx, %3
  mov r8, %4
  movq xmm2, r8		; silly
%else
  mov rdi, %2
  mov rsi, %3
  movsd xmm0, %4
%endif
  ALIGN_STACK
%ifdef FEATURE_WINDOWS
  sub rsp, 32
%else
  mov rax, 1
%endif
  call MANGLE(%1) WRTPLT
  RESTORE_STACK
  RESTORE
%endmacro

%macro LIBCALL4 5
  extern MANGLE(%1)
  SAVE
%ifdef FEATURE_WINDOWS
  mov rcx, %2
  mov rdx, %3
  mov r8, %4
  mov r9, %5
%else
  mov rdi, %2
  mov rsi, %3
  mov rdx, %4
  mov rcx, %5
%endif
  ALIGN_STACK
%ifdef FEATURE_WINDOWS
  sub rsp, 32
%else
  xor rax, rax
%endif
  call MANGLE(%1) WRTPLT
  RESTORE_STACK
  RESTORE
%endmacro

%macro SYSCALL0 1
  SAVE
  ALIGN_STACK
  mov rax, %1
  syscall
  RESTORE_STACK
  RESTORE
%endmacro

%macro SYSCALL1 2
  SAVE
  mov rdi, %2
  ALIGN_STACK
  mov rax, %1
  syscall
  RESTORE_STACK
  RESTORE
%endmacro

%macro SYSCALL2 3
  SAVE
  mov rdi, %2
  mov rsi, %3
  ALIGN_STACK
  mov rax, %1
  syscall
  RESTORE_STACK
  RESTORE
%endmacro

%macro SYSCALL3 4
  SAVE
  mov rdi, %2
  mov rsi, %3
  mov rdx, %4
  ALIGN_STACK
  mov rax, %1
  syscall
  RESTORE_STACK
  RESTORE
%endmacro

%macro SYSCALL4 5
  SAVE
  mov rdi, %2
  mov rsi, %3
  mov rdx, %4
  mov rcx, %5
  ALIGN_STACK
  mov rax, %1
  syscall
  RESTORE_STACK
  RESTORE
%endmacro


;; vector access check: %1 = block, %2 = index (fixnum)
%macro CHECK_SLOT_ACCESS 2
%ifdef FEATURE_CHECK
  push rax
  push r11
  mov rax, %1
  mov r11, %2
  call check_slot_access
  pop r11
  pop rax
%endif
%endmacro

;; vector access check: %1 = block, %2 = index (fixnum)
%macro CHECK_BYTE_ACCESS 2
%ifdef FEATURE_CHECK
  push rax
  push r11
  mov rax, %1
  mov r11, %2
  call check_byte_access
  pop r11
  pop rax
%endif
%endmacro

;; procedure check: SELF = block
%macro CHECK_PROCEDURE 0
%ifdef FEATURE_CHECK
  call check_procedure
%endif
%endmacro

;; argc check failed
%macro CHECK_ARGC_FAILED 0
  call check_argc_failed
%endmacro


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


section .text

%ifdef FEATURE_EMBEDDED
global MANGLE(ENTRYPOINT)
MANGLE(ENTRYPOINT):
  SAVE
  push rbp
  mov rax, [saved_k]
  test rax, rax
  if z
    jmp init
  endif
  mov FALSE, false
  mov ALLOC, [saved_ALLOC]
  mov LIMIT, [saved_LIMIT]
  mov [toplevel_rsp], rsp
  mov rcx, rdi			; argument
  mov SELF, rax			; saved K
  mov r11, 2
  mov rax, [SELF + CELLS(1)]
  jmp rax
%elifdef FEATURE_NOLIBC
global _start
_start:
  pop rdi			; argc
  mov [argc], rdi
  mov [argv], rsp		; point to argument array
  add rdi, 2
  lea rax, [rsp + rdi * CELLS(1)]
  mov [envp], rax
  mov rax, .exit
  push rax
  SAVE
  push rbp
  jmp init
.exit:
  SYSCALL1 60, rax		; sys_exit
%else
global MANGLE(main)
MANGLE(main):
  SAVE
  push rbp
%ifdef FEATURE_WINDOWS
  mov [argc], rcx
  mov [argv], rdx
%else
  mov [argc], rdi
  mov [argv], rsi
%endif
  jmp init
%endif


init:
  xor rax, rax
  mov [gc_count], rax
  mov FALSE, false
  mov ALLOC, [fromspace]
  mov LIMIT, [fromspace_end]
  sub LIMIT, FROMSPACE_RESERVE
  mov rcx, terminate_closure
  mov [toplevel_rsp], rsp
  mov r11, 2
  xor SELF, SELF	    ; current closure, empty here
  jmp toplevel


terminate:
  mov rsp, [toplevel_rsp]
  mov rax, [exit_code]
  FIX2INT rax
  pop rbp
  RESTORE
%ifdef FEATURE_EMBEDDED
  mov rax, premature_exit_error_object
%endif  
  ret


;; consrest: registers/locals = arguments, r11 = argc, rax = non-rest args -> rax (ptr)
consrest:
  push rcx			; save k
  mov rcx, null
  repeat
    cmp r11, rax
    je .done
    cmp r11, NUMBER_OF_ARGUMENT_REGISTERS
  while a
    push r11
    sub r11, NUMBER_OF_ARGUMENT_REGISTERS + 1
    mov [ALLOC + CELLS(2)], rcx
    mov rcx, PAIR | 2
    mov [ALLOC], rcx
    mov rcx, ALLOC
%ifdef FEATURE_PIC
    lea r15, [locals]
    mov r15, [r15 + r11 * CELLS(1)]
%else
    mov r15, [locals + r11 * CELLS(1)]
%endif
    mov [ALLOC + CELLS(1)], r15
    add ALLOC, CELLS(3)
    pop r11
    dec r11
  again
  sub r11, 2
%ifdef FEATURE_PIC
  lea r15, [rel consrest_jmptable]
  mov r15, [r15 + r11 * CELLS(1)]
  call .a0
.a0:
  add r15, [rsp]
  add rsp, CELLS(1)
%else
  mov r15, [consrest_jmptable + r11 * CELLS(1)]
%endif
  inc r11	      ;XXX get rid of this, probably by adjusting jmptable
  jmp r15
%macro CONSREST1 1
  mov r15, PAIR | 2
  mov [ALLOC], r15
  mov [ALLOC + CELLS(1)], %1
  mov [ALLOC + CELLS(2)], rcx
  mov rcx, ALLOC
  add ALLOC, CELLS(3)
  dec r11
  cmp r11, rax
  jb .done
%endmacro
.a8: CONSREST1 r12
.a7: CONSREST1 r10
.a6: CONSREST1 r9
.a5: CONSREST1 r8
.a4: CONSREST1 rdi
.a3: CONSREST1 rsi
.a2: CONSREST1 rdx
.a1:
.done:
  mov rax, rcx
  pop rcx
  ret

section .data

%ifdef FEATURE_PIC
%define CONSREST_OFF(lbl)  consrest. %+ lbl - consrest.a0
%else
%define CONSREST_OFF(lbl)  consrest. %+ lbl
%endif
consrest_jmptable:
  dq    CONSREST_OFF(a1)
  dq    CONSREST_OFF(a2)
  dq    CONSREST_OFF(a3)
  dq    CONSREST_OFF(a4)
  dq    CONSREST_OFF(a5)
  dq    CONSREST_OFF(a6)
  dq    CONSREST_OFF(a7)
  dq    CONSREST_OFF(a8)

section .text


;; =: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE numerically_equal
  mov r15, compare_numerically_equal
  jmp pairwise_compare

;; >: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE numerically_greater
  mov r15, compare_numerically_greater
  jmp pairwise_compare

;; <: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE numerically_less
  mov r15, compare_numerically_less
  jmp pairwise_compare

;; >=: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE numerically_greater_or_equal
  mov r15, compare_numerically_greater_or_equal
  jmp pairwise_compare

;; <=: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE numerically_less_or_equal
  mov r15, compare_numerically_less_or_equal
  jmp pairwise_compare


;; pairwise_compare: rcx = k, rdx... = arguments, r11 = argc, r15 = compare -> (k boolean)
;; the compare-function gets 2 arguments in rax + rbx and should set rax accordingly (and avoid changing any other registers)
;; returns #f if argc <= 1
pairwise_compare:
  cmp r11, 1
  if be
.no:
    CONTINUE FALSE
  endif
  mov rax, rdx			; 1st arg
  mov rbx, rsi			; 2nd arg
  call r15
  test al, al
  jz .no
  cmp r11, 4
  je .yes
  mov rax, rbx
  mov rbx, rdi			; 3rd arg
  call r15
  test al, al
  jz .no
  cmp r11, 5
  je .yes
  mov rax, rbx
  mov rbx, r8			; 4th arg
  call r15
  test al, al
  jz .no
  cmp r11, 6
  je .yes
  mov rax, rbx
  mov rbx, r9			; 5th arg
  call r15
  test al, al
  jz .no
  cmp r11, 7
  je .yes
  mov rax, rbx
  mov rbx, r10			; 6th arg
  call r15
  test al, al
  jz .no
  cmp r11, 8
  je .yes
  mov rax, rbx
  mov rbx, r12			; 7th arg
  call r15
  test al, al
  jz .no
  mov rax, rbx
  sub r11, NUMBER_OF_ARGUMENT_REGISTERS
  mov rdx, locals
  repeat
    test r11, r11
  while nz
    mov rbx, [rdx]		; 7+nth arg
    call r15
    test al, al
    jz .no
    dec r11
    add rdx, CELLS(1)
  again
.yes:
  SET_T rax
  CONTINUE rax


;; comparison functions

%macro COMPARE_NUMERICALLY 3
compare_numerically_%1:
  test rax, 1
  jz .l1
  test rbx, 1			; rax = fixnum
  jz .l2
  cmp rax, rbx			; rax, rbx = fixnum
  set%2 al
  ret
.l1:
  test rbx, 1			; rax = !fixnum
  jz .l3
  ; rax = !fixnum, rbx = fixnum
  FIX2INT rbx
  cvtsi2sd xmm1, rbx
  movsd xmm0, [rax + CELLS(1)]
.compare:
  ucomisd xmm0, xmm1
  ;; this architecture is beyond repair: we can not use the same
  ;; condition codes for integer and float comparison, as [U]COMISD
  ;; apparently sets the flags like an unsigned integer comparison...
  set%3 al
  ret  
.l2:
  ; rax = fixnum, rbx = !fixnum
  FIX2INT rax
  cvtsi2sd xmm0, rax
  movsd xmm1, [rbx + CELLS(1)]
  jmp .compare
.l3:
  movsd xmm0, [rax + CELLS(1)]	; rax, rbx = !fixnum
  movsd xmm1, [rbx + CELLS(1)]
  jmp .compare
%endmacro

COMPARE_NUMERICALLY equal, e, e
COMPARE_NUMERICALLY greater, g, a
COMPARE_NUMERICALLY less, l, b
COMPARE_NUMERICALLY greater_or_equal, ge, ae
COMPARE_NUMERICALLY less_or_equal, le, be


;; *: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE multiply_numbers
  cmp r11, 3
  if b
    CONTINUE FIX(1)		; zero arguments -> 1
  endif
  if e
    CONTINUE rdx			; 1 argument -> identity
  endif
  mov r15, multiply_2
  jmp fold_binary_operation

;; +: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE add_numbers
  cmp r11, 3
  if b
    CONTINUE FIX(0)		; zero arguments -> 0
  endif
  if e
    CONTINUE rdx			; 1 argument -> identity
  endif
  mov r15, add_2
  jmp fold_binary_operation

;; -: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE subtract_numbers
  cmp r11, 3
  if be
    test rdx, 1      ; 1 argument - negate, returns garbage with 0 arguments
    if nz
      neg rdx			;XXX can this overflow?
      add rdx, 2
      CONTINUE rdx
    else
      movsd xmm1, [rdx + CELLS(1)]
      movsd xmm0, [flonum_0 + CELLS(1)]
      subsd xmm0, xmm1
      CONTINUE_FLOAT xmm0
    endif
  endif
  mov r15, subtract_2
  jmp fold_binary_operation

;; /: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE divide_numbers
  cmp r11, 3
  if be
    test rdx, 1    ; 1 argument - reciprocal, returns garbage with 0 arguments
    if nz
      FIX2INT rdx
      cvtsi2sd xmm1, rdx
.l2:
      movsd xmm0, [flonum_1 + CELLS(1)]
      divsd xmm0, xmm1
      CONTINUE_FLOAT xmm0
    endif
    movsd xmm1, [rdx + CELLS(1)]
    jmp .l2
  endif
  mov r15, divide_2
  jmp fold_binary_operation


;; max: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE maximize_numbers
  cmp r11, 3
  if be
    CONTINUE rdx    ; 1 argument - return unchanged, returns garbage with 0 arguments
  endif
  mov r15, maximize_2
  jmp fold_binary_operation


;; min: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE minimize_numbers
  cmp r11, 3
  if be
    CONTINUE rdx    ; 1 argument - return unchanged, returns garbage witrh 0 arguments
  endif
  mov r15, minimize_2
  jmp fold_binary_operation


;; fold operation over arguments: rcx = k, rdx... = arguments, r11 = argc, r15 = operation -> (k boolean)
;; at least 2 arguments must be given
;; the operation-function gets 2 arguments in rax + rbx and returns a result in rax 
;;   (may point to "temporary_flonum", and should avoid changing any registers but rax and rdx, rsi)
fold_binary_operation:
  mov rax, rdx			; 1st arg
  mov rbx, rsi			; 2nd arg
  call r15
  cmp r11, 4
  if e
.done:
    test rax, 1			; check wether flonum and copy it
    jnz .l0
    mov rax, [temporary_flonum]
    mov [ALLOC], rax
    mov rax, [temporary_flonum + CELLS(1)]
    mov [ALLOC + CELLS(1)], rax
    mov rax, ALLOC
    add ALLOC, CELLS(2)
.l0:
    CONTINUE rax
  endif
  mov rbx, rdi			; 3rd arg
  call r15
  cmp r11, 5
  je .done
  mov rbx, r8			; 4th arg
  call r15
  cmp r11, 6
  je .done
  mov rbx, r9			; 5th arg
  call r15
  cmp r11, 7
  je .done
  mov rbx, r10			; 6th arg
  call r15
  cmp r11, 8
  je .done
  mov rbx, r12			; 7th arg
  call r15
  sub r11, NUMBER_OF_ARGUMENT_REGISTERS
  mov rdx, locals
  test r11, r11
  je .done
  repeat
    mov rbx, [rdx]		; 7+nth arg
    call r15
    add rdx, CELLS(1)
    dec r11
    jz .done
  again


;; sub-operations used for folding arithmetic
add_2:
  test rax, 1
  jz .l1
  test rbx, 1			; rax = fixnum
  jz .l2
  dec rax			; rax, rbx = fixnum
  add rax, rbx
  ret
.l1:
  test rbx, 1			; rax = !fixnum
  jz .l3
  ; rax = !fixnum, rbx = fixnum
  FIX2INT rbx
  cvtsi2sd xmm0, rbx
  movsd xmm1, [rax + CELLS(1)]
  addsd xmm0, xmm1
.done:
  movsd [temporary_flonum + CELLS(1)], xmm0
  mov rax, temporary_flonum
  ret
.l2:
  ; rax = fixnum, rbx = !fixnum
  FIX2INT rax
  cvtsi2sd xmm0, rax
  movsd xmm1, [rbx + CELLS(1)]
  addsd xmm0, xmm1
  jmp .done
.l3:
  movsd xmm0, [rax + CELLS(1)]	; rax, rbx = !fixnum
  movsd xmm1, [rbx + CELLS(1)]
  addsd xmm0, xmm1
  jmp .done

subtract_2:
  test rax, 1
  jz .l1
  test rbx, 1			; rax = fixnum
  jz .l2
  sub rax, rbx			; rax, rbx = fixnum
  inc rax
  ret
.l1:
  test rbx, 1			; rax = !fixnum
  jz .l3
  ; rax = !fixnum, rbx = fixnum
  FIX2INT rbx
  cvtsi2sd xmm1, rbx
  movsd xmm0, [rax + CELLS(1)]
  subsd xmm0, xmm1
.done:
  movsd [temporary_flonum + CELLS(1)], xmm0
  mov rax, temporary_flonum
  ret
.l2:
  ; rax = fixnum, rbx = !fixnum
  FIX2INT rax
  cvtsi2sd xmm0, rax
  movsd xmm1, [rbx + CELLS(1)]
  subsd xmm0, xmm1
  jmp .done
.l3:
  movsd xmm0, [rax + CELLS(1)]	; rax, rbx = !fixnum
  movsd xmm1, [rbx + CELLS(1)]
  subsd xmm0, xmm1
  jmp .done

multiply_2:
  test rax, 1
  jz .l1
  test rbx, 1			; rax = fixnum
  jz .l2
  FIX2INT rax			; rax, rbx = fixnum
  FIX2INT rbx
  imul rbx
  INT2FIX rax
  ret
.l1:
  test rbx, 1			; rax = !fixnum
  jz .l3
  ; rax = !fixnum, rbx = fixnum
  FIX2INT rbx
  cvtsi2sd xmm0, rbx
  movsd xmm1, [rax + CELLS(1)]
  mulsd xmm0, xmm1
.done:
  movsd [temporary_flonum + CELLS(1)], xmm0
  mov rax, temporary_flonum
  ret
.l2:
  ; rax = fixnum, rbx = !fixnum
  FIX2INT rax
  cvtsi2sd xmm0, rax
  movsd xmm1, [rbx + CELLS(1)]
  mulsd xmm0, xmm1
  jmp .done
.l3:
  movsd xmm0, [rax + CELLS(1)]	; rax, rbx = !fixnum
  movsd xmm1, [rbx + CELLS(1)]
  mulsd xmm0, xmm1
  jmp .done

divide_2:
  test rax, 1
  jz .l1
  test rbx, 1			; rax = fixnum
  jz .l2
  FIX2INT rax			; rax, rbx = fixnum
  FIX2INT rbx
  movq xmm0, rax
  movq xmm1, rbx
  divsd xmm0, xmm1
  jmp .done
  ret
.l1:
  test rbx, 1			; rax = !fixnum
  jz .l3
  ; rax = !fixnum, rbx = fixnum
  FIX2INT rbx
  cvtsi2sd xmm1, rbx
  movsd xmm0, [rax + CELLS(1)]
  divsd xmm0, xmm1
.done:
  movsd [temporary_flonum + CELLS(1)], xmm0
  mov rax, temporary_flonum
  ret
.l2:
  ; rax = fixnum, rbx = !fixnum
  FIX2INT rax
  cvtsi2sd xmm0, rax
  movsd xmm1, [rbx + CELLS(1)]
  divsd xmm0, xmm1
  jmp .done
.l3:
  movsd xmm0, [rax + CELLS(1)]	; rax, rbx = !fixnum
  movsd xmm1, [rbx + CELLS(1)]
  divsd xmm0, xmm1
  jmp .done


maximize_2:
  test rax, 1
  jz .l1
  test rbx, 1			; rax = fixnum
  jz .l2
  cmp rax, rbx			; rax, rbx = fixnum
  cmovl rax, rbx
  ret
.l1:
  test rbx, 1			; rax = !fixnum
  jz .l3
  ;; rax = !fixnum, rbx = fixnum
  mov rsi, rbx
  FIX2INT rsi
  cvtsi2sd xmm1, rsi
  movsd xmm0, [rax + CELLS(1)]
.fresult:
  ucomisd xmm0, xmm1
  if b
    movsd xmm0, xmm1
  endif
  movsd [temporary_flonum + CELLS(1)], xmm0
  mov rax, temporary_flonum
  ret
.l2:
  ;; rax = fixnum, rbx = !fixnum
  mov rsi, rax
  FIX2INT rsi
  cvtsi2sd xmm0, rsi
  movsd xmm1, [rbx + CELLS(1)]
  jmp .fresult
.l3:
  movsd xmm0, [rax + CELLS(1)]	; rax, rbx = !fixnum
  movsd xmm1, [rbx + CELLS(1)]
  jmp .fresult


minimize_2:
  test rax, 1
  jz .l1
  test rbx, 1			; rax = fixnum
  jz .l2
  cmp rax, rbx			; rax, rbx = fixnum
  cmovg rax, rbx
  ret
.l1:
  test rbx, 1			; rax = !fixnum
  jz .l3
  ;; rax = !fixnum, rbx = fixnum
  mov rsi, rbx
  FIX2INT rsi
  cvtsi2sd xmm1, rsi
  movsd xmm0, [rax + CELLS(1)]
.fresult:
  ucomisd xmm0, xmm1
  if a
    movsd xmm0, xmm1
  endif
  movsd [temporary_flonum + CELLS(1)], xmm0
  mov rax, temporary_flonum
  ret
.l2:
  ;; rax = fixnum, rbx = !fixnum
  mov rsi, rax
  FIX2INT rsi
  cvtsi2sd xmm0, rsi
  movsd xmm1, [rbx + CELLS(1)]
  jmp .fresult
.l3:
  movsd xmm0, [rax + CELLS(1)]	; rax, rbx = !fixnum
  movsd xmm1, [rbx + CELLS(1)]
  jmp .fresult


;; quotient: rax, r11 = numbers -> rax
quotient:
  test rax, 1
  jz .l0
  test r11, 1
  jz .l1
  FIX2INT rax			; rax = fixnum, r11 = fixnum
  FIX2INT r11
  push rdx
  cqo
  idiv r11
  pop rdx
  INT2FIX rax
  ret
.l0:
  test r11, 1
  jz .l2
  movsd xmm0, [rax + CELLS(1)]		; rax = flonum, r11 = fixnum
  FIX2INT r11
  cvtsi2sd xmm1, r11
.l3:
  divsd xmm0, xmm1
  ALLOC_FLONUM xmm0
  ret
.l2:
  movsd xmm0, [rax + CELLS(1)]		; rax = flonum, r11 = flonum
  movsd xmm1, [r11 + CELLS(1)]
  jmp .l3
.l1:
  FIX2INT rax			; rax = fixnum, r11 = flonum
  cvtsi2sd xmm0, rax
  movsd xmm1, [r11 + CELLS(1)]
  jmp .l3


;; remainder: rax, r11 = numbers -> rax 
remainder:
  test rax, 1
  if nz
    FIX2INT rax
    test r11, 1
    if nz
      FIX2INT r11		; rax, r11 = fixnum
      push rdx
      cqo
      idiv r11
      INT2FIX rdx
      mov rax, rdx
      pop rdx
      ret
    endif
    cvtsi2sd xmm0, rax		; rax = fixnum, r11 = flonum
    movsd xmm1, [r11 + CELLS(1)]
.l0:
    movsd xmm2, xmm0		; save
    divsd xmm0, xmm1
    subsd xmm2, xmm0
    ALLOC_FLONUM xmm2
    ret
  endif
  movsd xmm0, [rax + CELLS(1)]  	; rax = flonum
  test r11, 1
  if nz
    cvtsi2sd xmm1, r11    	; rax = flonum, r11 = fixnum
    jmp .l0
  endif
  movsd xmm1, [r11 + CELLS(1)]		; rax = flonum r11 = flonum
  jmp .l0


;; list_length: rax = lst -> rax, clobbers r11
;; stops on the first non-pair
list_length:
  xor r11, r11
  repeat
    test rax, 1
    jnz .l1
    cmp byte TYPENUMBER_REF(rax), TYPENUMBER(PAIR)
  while e
    inc r11
    mov rax, [rax + CELLS(2)]
  again
.l1:
  INT2FIX r11
  mov rax, r11
  ret


;; apply: rcx = k, rdx = proc, rsi... = args -> (k results ...)
PRIMITIVE apply
  mov rbx, rdx			; proc
  CHECK_PROCEDURE
  push r11
  cmp r11, NUMBER_OF_ARGUMENT_REGISTERS
  ja .l9
  sub r11, 4
%ifdef FEATURE_PIC
  lea r15, [apply_jmptable]
  mov rax, [r15 + r11 * CELLS(1)]
  call .l0
.l0:
  add rax, [rsp]
  add rsp, CELLS(1)
%else
  mov rax, [apply_jmptable + r11 * CELLS(1)]
%endif
  jmp rax
  ;; jmptable: move all register arguments into "tempregisters", starting from rsi
.l9:
  mov [tempregisters + CELLS(5)], r12
.l8:
  mov [tempregisters + CELLS(4)], r10
.l7:
  mov [tempregisters + CELLS(3)], r9
.l6:
  mov [tempregisters + CELLS(2)], r8
.l5:
  mov [tempregisters + CELLS(1)], rdi
.l4:
  mov [tempregisters], rsi
  ;; now deconstruct last argument
%ifdef FEATURE_PIC
  lea r15, [tempregisters]
  lea rdi, [r15 + r11 * CELLS(1)]
%else
  lea rdi, [tempregisters + r11 * CELLS(1)]
%endif
  mov rsi, [rdi]
  mov rdx, null
  pop r11
  sub r11, 2
  repeat
%ifdef FEATURE_CHECK
    cmp r11, MAXIMUM_NUMBER_OF_ARGUMENTS - 2
    if a
      call check_apply_limit_failed
    endif
%endif
    cmp rsi, rdx		; compare current list with '()
  while ne
    mov rax, [rsi + CELLS(1)]
    mov [rdi], rax
    add rdi, CELLS(1)
    mov rsi, [rsi + CELLS(2)]
    inc r11
  again
  ;; now move register arguments back
  mov rdx, [tempregisters]
  mov rsi, [tempregisters + CELLS(1)]
  mov rdi, [tempregisters + CELLS(2)]
  mov r8, [tempregisters + CELLS(3)]
  mov r9, [tempregisters + CELLS(4)]
  mov r10, [tempregisters + CELLS(5)]
  mov r12, [tempregisters + CELLS(6)]
  mov rax, [SELF + CELLS(1)]
  jmp rax

section .data

%ifdef FEATURE_PIC
%define APPLY_OFF(lbl)  apply. %+ lbl - apply.l0
%else
%define APPLY_OFF(lbl)  apply. %+ lbl
%endif
apply_jmptable:
  dq APPLY_OFF(l4)
  dq APPLY_OFF(l5)
  dq APPLY_OFF(l6)
  dq APPLY_OFF(l7)
  dq APPLY_OFF(l8)

section .text


;; invoke garbage collection: rcx = k -> (k void)
PRIMITIVE reclaim_garbage
  mov SELF, rcx
  mov rcx, undefined		; ignored
  mov r11, 2
  mov rax, [SELF + CELLS(1)]
  jmp reclaim


;; allocate block: rcx = k, rdx = typenumber, rsi = bytes, rdi = flag (bool), r8 = size, r9 = fill?, r10 = fillvalue -> (k object)
;; if heap-space is insufficient, trigger GC, and check for full heap afterwards
;; rdi holds flag set to #t when GC returns and re-enters this procedure
PRIMITIVE alloc_block
  cmp rdi, FALSE
  if e
    FIX2INT rsi
    mov rax, rsi
    add rax, CELLS(1)		; + header
    add rax, ALLOC
    cmp rax, LIMIT
    if ae
      ;; not enough memory, reclaim
      SET_T rdi
      INT2FIX rsi		; restore for reentry
      mov rax, [SELF + CELLS(1)] ; which is just "alloc_block"...
      jmp reclaim
    endif
.ok:
    ;; everything ok, alloc block
    FIX2INT rdx
    shl rdx, HEADER_SHIFT
    FIX2INT r8
    or rdx, r8
    mov [ALLOC], rdx
    mov rax, ALLOC
    add rsi, CELLS(1)
    add ALLOC, rsi
    ;; align
    add ALLOC, ALIGN_BASE
    and ALLOC, [nalign_base]
    ;; now fill block, so that it doesn't contain garbage pointers
    cmp r9, FALSE
    if ne
      push rcx
      push rax
      mov rcx, r8
      mov rdi, rax
      add rdi, CELLS(1)
      mov rax, r10
      rep stosq
      pop rax
      pop rcx
    endif
    CONTINUE rax
  else
    ;; check again for full heap
    FIX2INT rsi
    mov rax, rsi
    add rax, CELLS(1)		; + header
    add rax, ALLOC
    cmp rax, LIMIT
    jb .ok
    ;; not enough memory, trap
    call heap_full_trap
    CRASH   
  endif


;; values: rcx = k, rdx... = args -> (original-k args ...) or (k arg1)
PRIMITIVE values
  ;; check for values_continuation
  mov r15, values_continuation
  cmp r15, [rcx + CELLS(1)]
  if e
    ;; extract consumer
    mov SELF, [rcx + CELLS(3)]
    ;; extract original k
    mov rcx, [rcx + CELLS(2)]
    ;, and invoke
    mov rax, [SELF + CELLS(1)]
    jmp rax
  endif
  ;; if 0 args, pass void
  cmp r11, 2
  if e
    mov rdx, undefined
  endif
  mov r11, 2			; adjust argc
  CONTINUE rdx


;; call/values: rcx = k, rdx = producer, rsi = consumer -> (producer <values-k>)
PRIMITIVE call_with_values
  ;; wrap k + consumer into values-k
  mov rax, CLOSURE | 3
  mov [ALLOC], rax
  mov rax, values_continuation
  mov [ALLOC + CELLS(1)], rax
  mov [ALLOC + CELLS(2)], rcx
  mov [ALLOC + CELLS(3)], rsi
  mov rcx, ALLOC
  add ALLOC, CELLS(4)
  ;; call producer
  mov SELF, rdx
  mov rax, [SELF + CELLS(1)]
  mov r11, 2
  jmp rax

values_continuation:
  mov rdx, rcx			; result -> 1st arg
  mov rcx, [SELF + CELLS(2)]  ;	 extract original k
  ;; extract consumer and invoke it with a single argument
  mov SELF, [SELF + CELLS(3)]
  mov r11, 3
  mov rax, [SELF + CELLS(1)]
  jmp rax


;; call/cc: rcx = k, rdx = proc -> (proc k <call/cc-wrapper-proc>)
PRIMITIVE call_cc
  mov SELF, rdx			; save proc
  ;; build continuation-wrapper
  mov rax, CLOSURE | 2
  mov [ALLOC], rax
  mov rax, call_cc_wrapper
  mov [ALLOC + CELLS(1)], rax
  mov [ALLOC + CELLS(2)], rcx
  mov rdx, ALLOC
  add ALLOC, CELLS(3)
  ;; invoke proc
  mov rax, [SELF + CELLS(1)]
  mov r11, 3
  jmp rax

;; call/cc wrapper procedure: rcx = k, rdx... = args -> (original-k args ...)
call_cc_wrapper:
  ;; extract original k
  mov rcx, [SELF + CELLS(2)]
  ;; check for values_continuation
  mov r15, values_continuation
  cmp r15, [rcx + CELLS(1)]
  if e
    ;; extract consumer
    mov SELF, [rcx + CELLS(3)]
    ;; extract original k
    mov rcx, [rcx + CELLS(2)]
    ;, and invoke
    mov rax, [SELF + CELLS(1)]
    jmp rax
  endif
  ;; pass 1st arg (or void) as result
  cmp r11, 2
  if e
    mov rdx, undefined
  endif
  mov r11, 2			; adjust argc
  CONTINUE rdx  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; called when WRITE_BARRIER detects a write outside of the heap
write_barrier_trap:
  mov rax, error_msg_1
  mov r11, 3
  jmp invoke_error


;; called when ALLOC > LIMIT right after a GC (reserve is already subtracted from LIMIT)
;; may fail, if not enough heap is left
heap_full_trap:
  mov rax, error_msg_2
  mov r11, 3
  jmp invoke_error


;; write error message and exit: rax = raw string, r11 = length
write_error_and_exit:
%ifdef FEATURE_NOLIBC
  SYSCALL3 1, 2, rax, r11
%else
 %ifdef FEATURE_WINDOWS
  LIBCALL3 _write, 2, rax, r11	
 %else
  LIBCALL3 write, 2, rax, r11	
 %endif
%endif
  mov rax, FIX(70)			; EXIT_FAILURE
  mov [exit_code], rax
  jmp terminate


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
  and rcx, [size_mask]
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
;
; a simple Cheney-style semispace collector
;
;
; * roots:
;
;   locals (r11 - NUMBER_OF_ARGUMENT_REGISTERS)
;   registers (r11)
;   globals ... endglobals


;; mark object pointed to by rax, fixnum- or foreign-object-case inlined to reduce call overhead, clobbers r15
%macro MARK1 0
  mov r15, [rax]
  test r15, 1			; fixnum?
  if z 
    cmp r15, [fromspace]	; ignore if outside of fromspace
    jb %%1
    cmp r15, [fromspace_end]
    jae %%1
    call mark
  endif
%%1:
%endmacro


;; reclaim: rax = codeptr, r11 = number of live registers
reclaim:
  ;; increase gc-counter
  inc qword [gc_count]
  push rax
  push r11
%ifdef ENABLE_GC_LOGGING
  mov rax, gc_log_format
  mov r11, [gc_count]
  mov r15, [fromspace_end]
  sub r15, ALLOC
  call format_string
%endif
  mov r11, [rsp]
  mov rax, [rsp + CELLS(1)]
  ;; save registers
  mov [gcsave], rbx
  mov [gcsave + 1 * CELLS(1)], rcx
  mov [gcsave + 2 * CELLS(1)], rdx
  mov [gcsave + 3 * CELLS(1)], rsi
  mov [gcsave + 4 * CELLS(1)], rdi
  mov [gcsave + 5 * CELLS(1)], r8
  mov [gcsave + 6 * CELLS(1)], r9
  mov [gcsave + 7 * CELLS(1)], r10
  mov [gcsave + 8 * CELLS(1)], r12
  ;; setup variables for GC
  mov rdi, [tospace]	; rdi = tospace-top
  mov rsi, rdi			; rsi = scan-ptr
  ;; mark saved registers + locals
  ;; we cleverly ordered "gcsave", "tempregisters" and "locals" to mark all saved data in one go
  mov rax, gcsave
  repeat
    test r11, r11
  while nz
    MARK1
    add rax, CELLS(1)
    dec r11
  again
  ;; mark global variables
  mov rax, globals
  mov r8, endglobals
  repeat
    cmp rax, r8
  while b
    MARK1
    add rax, CELLS(1)
  again
  ;; now follow scan-ptr until it is equal to tospace-top
  repeat
    cmp rsi, rdi
  while b
    ;; ALIGNMENT: on 32-bit systems, skip alignment-hole marker
    mov rax, [rsi]		; get header
    mov rcx, rax		; rcx = block size
    and rcx, [size_mask]
    test rax, [byteblock_bit]
    if nz
      add rcx, CELLS(1)		; binary block, just skip
      add rcx, ALIGN_BASE	; align
      and rcx, [nalign_base]
      add rsi, rcx
    else
      ;; if closure, skip codeptr
      test rax, [special_bit]
      if nz
        add rsi, CELLS(1)
	dec rcx
      endif
      ;; mark items in block
      mov rax, rsi
      add rax, CELLS(1)
      inc rcx			; add 1, will be decremented at loop start
      repeat
        dec rcx
      while nz
        MARK1			; also depends on mark not clobbering rax
	add rax, CELLS(1)
      again
      mov rsi, rax
    endif
  again
  ;; swap spaces
  mov rdx, [fromspace]
  mov rax, [tospace]
  mov [fromspace], rax
  mov [tospace], rdx
  mov rdx, [fromspace_end]
  mov rax, [tospace_end]
  mov [fromspace_end], rax
  mov [tospace_end], rdx
  ;; update allocation-pointer and limit
  mov ALLOC, rdi
  mov LIMIT, [fromspace_end]
  sub LIMIT, FROMSPACE_RESERVE
%ifdef ENABLE_GC_LOGGING
  mov rax, gc_log_format2
  mov r11, LIMIT
  sub r11, ALLOC
  call format_string
%endif
  cmp ALLOC, LIMIT
  if ae 
    call heap_full_trap
    CRASH
  endif
  ;; restore registers
  pop r11
  pop rax
  mov rbx, [gcsave]
  mov rcx, [gcsave + 1 * CELLS(1)]
  mov rdx, [gcsave + 2 * CELLS(1)]
  mov rsi, [gcsave + 3 * CELLS(1)]
  mov rdi, [gcsave + 4 * CELLS(1)]
  mov r8, [gcsave + 5 * CELLS(1)]
  mov r9, [gcsave + 6 * CELLS(1)]
  mov r10, [gcsave + 7 * CELLS(1)]
  mov r12, [gcsave + 8 * CELLS(1)]
  ;; check whether interrupts are pending
  mov r15, [pending_signals]
  test r15, r15
  if nz
    mov SELF, [____25interrupt_2dhook]
    mov rcx, FALSE		; no continuation
    mov rax, [SELF + CELLS(1)]
    mov r11, 2
    jmp rax
  endif
  ;; continue with procedure call
  jmp rax


;; mark: rax = address of pointer to non-fixnum object, r15 = pointer to object, clobbers rbx, rdx, must not change rax
mark:
  mov rbx, [r15]		; rbx = header
  ;; check if already marked
  mov rdx, MARK_BIT
  test rbx, rdx
  if nz
    not rdx			; extract forwarding pointer
    and rbx, rdx
    mov [rax], rbx ; write forwarding pointer to address holding object-ptr
    ret
  endif
  push rcx
  ;; compute size
  mov rcx, rbx
  and rcx, [size_mask]
  test rbx, [byteblock_bit]
  if nz
    add rcx, ALIGN_BASE			; align
    shr rcx, CELL_SHIFT			; bytes -> words
  endif
  ;; create forwarding ptr and copy object to tospace
  ;; ALIGNMENT: on 32-bit systems, insert alignment-hole marker, if value is a flonum
  mov [rdi], rbx		; write header to tospace
  mov rdx, rdi
  or rdx, [mark_bit]		; mark header and install forwarding ptr
  mov [r15], rdx
  mov [rax], rdi		; modify original ptr to point to new object
  add rdi, CELLS(1)
  test rcx, rcx
  if z
    pop rcx
    ret				; empty block
  endif
  push rsi
  mov rsi, r15
  add rsi, CELLS(1)
  rep movsq
  pop rsi
  pop rcx
  ret


;; fill block with bytes: rax = block, r11 = byte -> rax, clobbers r15
fill_bytes:
  push rax
  push rcx
  push rsi
  push rdi
  mov rcx, [rax]
  and rcx, [size_mask]
  shl rcx, CELL_SHIFT
  lea rdi, [rax + CELLS(1)]
  mov rax, r11
  FIX2INT rax
  rep stosb
  pop rdi
  pop rsi
  pop rcx
  pop rax
  ret


;; format string using sprintf(3) and write to stderr: rax = raw format-string, r11, r15 = args
format_string:
%ifndef FEATURE_NOLIBC
  LIBCALL4 sprintf, buffer, rax, r11, r15
 %ifdef FEATURE_WINDOWS
  LIBCALL3 _write, 2, buffer, rax
 %else
  LIBCALL3 write, 2, buffer, rax
 %endif
%endif
  ret


;; fill block with pointers: rax = block, r11 = value -> rax, clobbers r15
fill_slots:
  push rax
  push rcx
  push rdi
  mov rcx, [rax]
  and rcx, [size_mask]
  if nz
    lea rdi, [rax + CELLS(1)]
    repeat
      WRITE_BARRIER [rdi], r11
      add rdi, CELLS(1)
      dec rcx
    until z
  endif
  pop rdi
  pop rcx
  pop rax
  ret


;; copy bytes from one block to another: rax = (source . src-pos), r11 = (dest . dst-pos), r15 = length -> rax = dest
copy_bytes:
  push rsi
  push rdi
  push rcx
  push rbx
  FIX2INT r15
  mov rcx, r15
  mov rbx, rax
  mov rsi, [rbx + CELLS(1)]
  mov rax, [rbx + CELLS(2)]
  FIX2INT rax
  add rax, CELLS(1)
  add rsi, rax
  mov rbx, r11
  mov rdi, [rbx + CELLS(1)]
  mov rax, [rbx + CELLS(2)]
  FIX2INT rax
  add rax, CELLS(1)
  add rdi, rax
  rep movsb
  pop rbx
  pop rcx
  pop rdi
  pop rsi
  ret  


;; copy slots from one block to another: rax = (source . src-pos), r11 = (dest . dst-pos), r15 = length -> rax = dest
copy_slots:
  push rbx
  push rcx
  push rdx
  FIX2INT r15
  mov rbx, [rax + CELLS(1)]	; (car src)
  mov rax, [rax + CELLS(2)]	; (cdr src)
  FIX2INT rax
  inc rax			; header
  shl rax, CELL_SHIFT
  add rbx, rax
  mov rcx, [r11 + CELLS(1)]	; (car dest)
  mov rax, [r11 + CELLS(2)]	; (cdr dest)
  FIX2INT rax
  inc rax			; header
  shl rax, CELL_SHIFT
  add rcx, rax
  test r15, r15
  if nz
    repeat
      mov rdx, [rbx]
      WRITE_BARRIER [rcx], rdx
      add rbx, CELLS(1)
      add rcx, CELLS(1)
      dec r15
    until z
  endif
  pop rdx
  pop rcx
  pop rbx
  ret  


;; fixnum (expt rax r11) -> rax
;; only suitable for small positive values of r11
fixnum_expt:
  FIX2INT rax
  FIX2INT r11
  mov r15, rax
  dec r11
  jz .done
  push rdx
  repeat
    imul r15
    dec r11
  until z
  pop rdx
.done:
  INT2FIX rax
  ret


;; compute flonum (expt rax r11) -> rax
;; adapted from http://stackoverflow.com/questions/4638473/how-to-powreal-real-in-x86
;; only handles positive exponents
flonum_expt:
  test r11, 1
  if nz
    FIX2INT r11
    mov [temporary_flonum + CELLS(1)], r11
    fild qword [temporary_flonum + CELLS(1)]
  else
    fld qword [r11 + CELLS(1)]
  endif
  lea r11, [temporary_flonum + CELLS(1)]
  test rax, 1
  if nz
    FIX2INT rax
    mov [r11], rax
    fild qword [r11]
  else
    fld qword [rax + CELLS(1)]
  endif
  fyl2x				; ST: 1
  ;; faster than adjusting the rounding mode and using x87 integer store
  fld st0			; ST: 2
  fisttp qword [r11]		; requires SSE3, ST: 1
  fild qword [r11]		; ST: 2
  fsub				; ST: 1
  f2xm1				
  fld1				; ST: 2
  fadd				; ST: 1
  fild qword [r11]		; ST: 2
  fxch
  fscale
  fstp qword [r11]		; ST: 1
  fstp qword [buffer]		; silly, but I don't know to to pop the FPU stack properly
  mov rax, FLONUM | CELLS(1)
  mov [ALLOC], rax
  mov rax, [r11]
  mov [ALLOC + CELLS(1)], rax
  mov rax, ALLOC
  add ALLOC, CELLS(2)
  ret


;; compare two objects structurally: rax, r11 = args -> rax (bool), clobbers r15
structurally_equal:
  ;; check whether both are eq?
  cmp rax, r11
  if e		
    SET_T rax	
    ret
  endif
  ;; check for one being immediate
  test rax, 1
  if nz
.fail:
    mov rax, FALSE	
    ret
  endif
  test r11, 1
  jnz .fail
  ;; compare headers
  mov r15, [rax]
  cmp [r11], r15
  jne .fail
  ;; compare contents
  push rcx
  push rsi
  push rdi
  mov rcx, r15
  and rcx, [size_mask]
  if z
    SET_T rax
    jmp .l1
  endif
  test r15, [byteblock_bit]
  if z
    shl rcx, CELL_SHIFT			; words -> bytes
  endif
  lea rsi, [rax + CELLS(1)]
  lea rdi, [r11 + CELLS(1)]
  repe cmpsb
  mov rax, FALSE
  if e
    SET_T rax
  endif
.l1:
  pop rdi
  pop rsi
  pop rcx
  ret


;; compare two objects recursively: rax, r11 = args -> rax (bool), clobbers r15
recursively_equal:
  ;; check whether both are eq?
  cmp rax, r11
  if e		
    SET_T rax	
    ret
  endif
  ;; check for one being immediate
  test rax, 1
  if nz
.fail:
    mov rax, FALSE	
    ret
  endif
  test r11, 1
  jnz .fail
  ;; compare headers
  mov r15, [rax]
  cmp [r11], r15
  jne .fail
  ;; compare contents
  push rcx
  push rsi
  push rdi
  mov rcx, r15
  and rcx, [size_mask]
  if z				; zero size?
    SET_T rax
    jmp .done
  endif
  test r15, [byteblock_bit]
  if z
    ;; non-byte block, compare elements
    add rax, CELLS(1)		; skip headers
    add r11, CELLS(1)
    repeat
      dec rcx
    while nz
      add rax, CELLS(1)
      push rax
      mov rax, [rax - CELLS(1)]
      add r11, CELLS(1)
      push r11
      mov r11, [r11 - CELLS(1)]
      push rcx
      call recursively_equal
      pop rcx
      pop r11
      cmp rax, FALSE
      pop r15
      je .done
      mov rax, r15
    again
    ;; do tail call for last element
    mov rax, [rax]
    mov r11, [r11]
    pop rdi
    pop rsi
    pop rcx
    jmp recursively_equal
  endif
  lea rsi, [rax + CELLS(1)]
  lea rdi, [r11 + CELLS(1)]
  repe cmpsb
  mov rax, FALSE
  if e
    SET_T rax
  endif
.done:
  pop rdi
  pop rsi
  pop rcx
  ret


;; debugging hook: rax -> rax
debug_hook:
  ret


;;; hashing function: rax = string -> rax (fixnum, 8-bit hash), clobbers r11, r15
hash_string:
  push rcx
  mov rcx, [rax]
  and rcx, [size_mask]
  if z
    mov r11, FIX(0)
    pop rcx
    ret
  endif
  add rax, CELLS(1)
  mov r11, rax
  xor rax, rax
  repeat
    movzx r15, byte [r11]
    xor rax, r15
%ifdef FEATURE_PIC
    lea r15, [random_numbers]
    movzx rax, byte [r15 + rax]
%else
    movzx rax, byte [random_numbers + rax]
%endif
    inc r11
    dec rcx
  until z
  pop rcx
  INT2FIX rax
  ret


;; basic string-comparison: rax, r11 = strings, r15 = length -> rax (fixnum 0, 1 or -1)
compare_strings:
  push rcx
  push rsi
  push rdi
  lea rsi, [rax + CELLS(1)]
  lea rdi, [r11 + CELLS(1)]
  mov rcx, r15
  FIX2INT rcx
  repe cmpsb
  if e 				; all characters compared
    mov rax, FIX(0)
  else
    mov al, [rsi - 1]
    cmp al, [rdi - 1]
    mov rax, FIX(1)
    mov r15, FIX(-1)
    cmovl rax, r15
  endif
  pop rdi
  pop rsi
  pop rcx
  ret      


;; case-insensitive string-comparison: rax, r11 = strings, r15 = length -> rax (fixnum 0, 1 or -1)
compare_strings_ci:
  push rsi
  push rdi
  push rbx
  lea rsi, [rax + CELLS(1)]
  lea rdi, [r11 + CELLS(1)]
  FIX2INT r15
  test r15, r15
  if nz
    repeat
      mov al, [rsi]
      cmp al, 'A'
      if ge			;XXX this can surely be done in a better way
        cmp al, 'Z'
	if le
	  or al, 0x20
	endif
      endif
      mov bl, [rdi]
      cmp bl, 'A'
      if ge			;XXX s.a.
        cmp bl, 'Z'
	if le
	  or bl, 0x20
	endif
      endif
      cmp al, bl
      if ne
        mov r15, FIX(-1)
	mov rax, FIX(1)
        cmovl rax, r15
    	jmp .done 
      endif  
      inc rsi
      inc rdi
      dec r15
    until z
  endif
  mov rax, FIX(0)
.done:
  pop rbx
  pop rdi
  pop rsi
  ret      


;; assoc: rax = object, r11 = list -> rax (pair or false), clobbers r15
;;         %1 = address of comparison subroutine, gets rax + pair in r15 and sets Z flag
;; handles non-pair list elements
%macro ASSOC 1
  push rsi
  push rcx
  push rdx
  mov rcx, PAIR | 2		; used for type-test
  mov rsi, r11
  repeat
    test rsi, 1			; list ends in fixnum?
    if nz
      mov rax, FALSE
      jmp %%1
    endif
    mov r15, [rsi]		; get header
    cmp r15, rcx		; check for pair
    if ne
      mov rax, FALSE
      jmp %%1
    endif
    mov r15, [rsi + CELLS(1)]	; get car
    test r15, 1			; fixnum element?
    if z
      mov rdx, [r15]		; get element header
      cmp rdx, rcx		; check for pair
      if e
        call %1
        if e
          mov rax, r15		; done
  	  jmp %%1
	endif
      endif
    endif
    mov rsi, [rsi + CELLS(2)]	; get cdr
  again
%%1:
  pop rdx
  pop rcx
  pop rsi
%endmacro


;; assq: rax = object, r11 = list -> rax (pair or false), clobbers r15
;; handles non-pair list elements
assoc_eq:
  ASSOC assoc_cmp_eq
  ret

assoc_cmp_eq:
  cmp rax, [r15 + CELLS(1)]
  ret


;; assv: rax = object, r11 = list -> rax (pair or false), clobbers r15
;; handles non-pair list elements
assoc_eqv:
  ASSOC assoc_cmp_eqv
  ret

assoc_cmp_eqv:
  push r11
  push r15
  push rax
  mov r11, [r15 + CELLS(1)]
  call structurally_equal
  SET_T r11
  cmp rax, r11
  pop rax
  pop r15
  pop r11
  ret


;; assoc: rax = object, r11 = list -> rax (pair or false), clobbers r15
;; handles non-pair list elements
assoc_equal:
  ASSOC assoc_cmp_equal
  ret

assoc_cmp_equal:
  push r11
  push r15
  push rax
  mov r11, [r15 + CELLS(1)]
  call recursively_equal
  SET_T r11
  cmp rax, r11
  pop rax
  pop r15
  pop r11
  ret


;; member: rax = object, r11 = list -> rax (pair or false), clobbers r15
;;         %1 = address of comparison subroutine, gets rax + car in r15 and sets Z flag
%macro MEMBER 1
  push rsi
  push rcx
  push rdx
  mov rcx, PAIR | 2		; used for type-test
  mov rsi, r11
  repeat
    test rsi, 1			; list ends in fixnum?
    if nz
      mov rax, FALSE
      jmp %%1
    endif
    mov r15, [rsi]		; get header
    cmp r15, rcx		; check for pair
    if ne
      mov rax, FALSE
      jmp %%1
    endif
    mov r15, [rsi + CELLS(1)]	; get car
    call %1
    if e
      mov rax, rsi		; done
      jmp %%1
    endif
    mov rsi, [rsi + CELLS(2)]	; get cdr
  again
%%1:
  pop rdx
  pop rcx
  pop rsi
%endmacro


;; memq: rax = object, r11 = list -> rax (pair or false), clobbers r15
member_eq:
  MEMBER member_cmp_eq
  ret

member_cmp_eq:
  cmp rax, r15
  ret


;; memv: rax = object, r11 = list -> rax (pair or false), clobbers r15
member_eqv:
  MEMBER member_cmp_eqv
  ret

member_cmp_eqv:
  push r11
  push r15
  push rax
  mov r11, r15
  call structurally_equal
  SET_T r11
  cmp rax, r11
  pop rax
  pop r15
  pop r11
  ret


;; member: rax = object, r11 = list -> rax (pair or false), clobbers r15
member_equal:
  MEMBER member_cmp_equal
  ret

member_cmp_equal:
  push r11
  push r15
  push rax
  mov r11, r15
  call recursively_equal
  SET_T r11
  cmp rax, r11
  pop rax
  pop r15
  pop r11
  ret


;; return to host program: rcx = k, rdx = result
;; crashes if used and not embedded
%ifdef FEATURE_EMBEDDED
return_to_host:
  mov [saved_k], rcx
  mov [saved_ALLOC], ALLOC
  mov [saved_LIMIT], LIMIT
  mov rax, rdx
  mov rsp, [toplevel_rsp]
  pop rbp
  RESTORE
  ret
%endif


;; convert string to number: rax = string, r11 = base -> rax (number)
%ifndef FEATURE_NOLIBC
str2num:
  push rdx
  push rbx
  call copy_to_buffer
  ;; check whether the string has length zero
  mov dl, [buffer]
  test dl, dl
  if z
    mov rax, FALSE
    jmp .done
  endif
  push rax			; endptr
  mov rdx, buffer
  mov r15, rsp
  FIX2INT r11
%ifdef FEATURE_LLP64
  LIBCALL3 strtoll, rdx, r15, r11
%else
  LIBCALL3 strtol, rdx, r15, r11
%endif
  ;; check endptr for being '\0'
  pop r11
  mov bl, [r11]
  test bl, bl
  if z
    ;; it is an integer
    INT2FIX rax
  else
    ;; now try if it is a float
    mov rax, buffer
    push rax			; endptr
    mov r15, rsp
    LIBCALL2 strtod, buffer, r15
    pop r11
    mov bl, [r11]
    test bl, bl
    if z
      mov rax, FLONUM | CELLS(1)
      mov [ALLOC], rax
      movsd [ALLOC + CELLS(1)], xmm0
      mov rax, ALLOC
      add ALLOC, CELLS(2)
    else
      mov rax, FALSE
    endif
  endif
.done:
  pop rbx
  pop rdx
  ret


;; convert number to string: rax = number, r11 = base -> rax (string)
num2str:
  push rdx
  FIX2INT r11
  test rax, 1
  if nz
    cmp r11, 8
    if e
      mov r15, ocvt
    else
      cmp r11, 16
      if e
        mov r15, xcvt
      else
        mov r15, dcvt
      endif
    endif
    FIX2INT rax
    LIBCALL3 sprintf, buffer, r15, rax
  else
    LIBCALL3_1 sprintf, buffer, gcvt, [rax + CELLS(1)]
  endif
  mov rax, buffer
  call alloc_zstring
  pop rdx
  ret    
%endif


;; get string representation of "errno": -> rax (string)
%ifndef FEATURE_NOLIBC
 %ifdef  FEATURE_WINDOWS
  %define GET_ERRNO_LOCATION  _errno
 %elifdef FEATURE_BSD
  %define GET_ERRNO_LOCATION  __errno
 %elifdef FEATURE_MAC
  %define GET_ERRNO_LOCATION  __error
 %else
  %define GET_ERRNO_LOCATION __errno_location
 %endif
get_last_error:
  LIBCALL0 GET_ERRNO_LOCATION
  mov eax, dword [rax]
  LIBCALL1 strerror, rax
  call alloc_zstring
  ret  
%endif


;; lookup char in character-table: rax = code (fixnum) -> rax (char or #f)
lookup_char:
  FIX2INT rax
  cmp rax, 256
  if b
    lea r11, [char_table]
    shl rax, CELL_SHIFT + 1
    add rax, r11
    ret
  endif
  mov rax, FALSE
  ret


;; invoke "(%error MSG ARGS ...)": rax = error-msg (char *), rsi, ... = irritants, r11 = argc
invoke_error:
  mov rsp, [toplevel_rsp]		; just in case error was triggered with stuff on the stack
  call alloc_zstring
  mov rdx, rax
  mov SELF, [____25error]
  mov rcx, FALSE		; no continuation
  mov rax, [SELF + CELLS(1)]
  jmp rax


%ifdef FEATURE_CHECK


;; check slot-access: rax = block, r11 = index (fixnum), clobbers r11
check_slot_access:
  push r15
  test rax, 1
  jnz .fail
  test r11, 1
  jz .fail
  mov r15, [rax]
  test r15, [byteblock_bit]
  if z
    and r15, [size_mask]
    FIX2INT r11
    cmp r11, r15
    if b
      pop r15
      ret
    endif
  endif
.fail:
  mov rsi, rax
  mov rdi, r11
  mov rax, error_msg_3
  mov r11, 5
  jmp invoke_error


;; check byte-access: rax = block, r11 = index (fixnum), clobbers r11
check_byte_access:
  push r15
  test rax, 1
  jnz .fail
  test r11, 1
  jz .fail
  mov r15, [rax]
  test r15, [byteblock_bit]
  if nz
    and r15, [size_mask]
    FIX2INT r11
    cmp r11, r15
    if b
      pop r15
      ret
    endif
  endif
.fail:
  mov rsi, rax
  mov rdi, r11
  mov rax, error_msg_4
  mov r11, 5
  jmp invoke_error


;; check procedure: SELF = block
check_procedure:
  test SELF, 1
  jnz .fail
  push rax
  mov rax, [SELF]
  and rax, [bits_mask]
  cmp rax, [closure_type]
  if e
    pop rax
    ret
  endif
.fail:
  mov rsi, SELF
  mov rax, error_msg_5
  mov r11, 4
  jmp invoke_error


;; argc check failed: r11 = argc, SELF = procedure
check_argc_failed:
  mov rsi, SELF
  mov rdi, r11
  INT2FIX rdi
  mov rax, error_msg_7
  mov r11, 5
  jmp invoke_error

;; argc-limit check in "apply" failed: r11 = argc, SELF = procedure
check_apply_limit_failed:
  mov rsi, SELF
  mov rdi, r11
  INT2FIX rdi
  mov rax, error_msg_6
  mov r11, 5
  jmp invoke_error


%endif

;; global generic signal handler
signal_handler:
  push rcx
  mov rcx, rdi
  mov rax, 1
  dec rcx
  shl rax, cl
  or [pending_signals], rax
  pop rcx
  ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


section .data

fromspace: dq area1
fromspace_end: dq area1 + TOTAL_HEAP_SIZE / 2
tospace: dq area2
tospace_end: dq area2 + TOTAL_HEAP_SIZE / 2
fromspace_limit: dq area1 + FROMSPACE_RESERVE
exit_code: dq FIX(0)

align 8

undefined: dq VOID | 0

null: dq NULL | 0

eof: dq EOF | 0

false:
  dq BOOLEAN | 1
  dq FIX(0)

true:
  dq BOOLEAN | 1
  dq FIX(1)

terminate_closure:
  dq CLOSURE | 1
  dq terminate

%ifdef FEATURE_EMBEDDED
premature_exit_error_object:
  dq RECORD | 6
  dq error_object_symbol, FIX(1), premature_exit_error_msg, null, false, false
premature_exit_error_msg:
  dq STRING | (.msg2 - .msg1)
.msg1:
  db `premature exit in embedded code`
.msg2:
;; note: this is not eq? to (string->symbol "error-object")
error_object_symbol:
  dq SYMBOL | 1
  dq error_object_string
error_object_string:
  dq STRING | 12
  db `error-object`
%endif

temporary_flonum: dq FLONUM | CELLS(1), 0
flonum_0: dq FLONUM | CELLS(1), __float64__(0.0)
flonum_1: dq FLONUM | CELLS(1), __float64__(1.0)
ieee754_nan: dq FLONUM | CELLS(1), 0x7ff0000000000001
ieee754_inf: dq FLONUM | CELLS(1), 0x7ff0000000000000
ieee754_ninf: dq FLONUM | CELLS(1), 0xfff0000000000000
argc: dq 0
saved_k: dq 0

error_msg_1: db "store to non-heap data detected", 0
error_msg_2: db "out of memory", 0
error_msg_3: db "invalid slot access", 0
error_msg_4: db "invalid byte access", 0
error_msg_5: db "call of non procedure", 0
error_msg_6: db "apply: too many arguments", 0
error_msg_7: db "wrong number of arguments", 0
error_msg_8:

gc_log_format: db "[GC #%d, reserve: %d bytes ...", 0
gc_log_format2: db " remaining: %d bytes]", 10, 0

random_numbers:
  db 98,6,85,150,36,23,112,164,135,207,169,5,26,64,165,219
  db 61,20,68,89,130,63,52,102,24,229,132,245,80,216,195,115
  db 90,168,156,203,177,120,2,190,188,7,100,185,174,243,162,10
  db 237,18,253,225,8,208,172,244,255,126,101,79,145,235,228,121
  db 123,251,67,250,161,0,107,97,241,111,181,82,249,33,69,55
  db 59,153,29,9,213,167,84,93,30,46,94,75,151,114,73,222
  db 197,96,210,45,16,227,248,202,51,152,252,125,81,206,215,186
  db 39,158,178,187,131,136,1,49,50,17,141,91,47,129,60,99
  db 154,35,86,171,105,34,38,200,147,58,77,118,173,246,76,254
  db 133,232,196,144,198,124,53,4,108,74,223,234,134,230,157,139
  db 189,205,199,128,176,19,211,236,127,192,231,70,233,88,146,44
  db 183,201,22,83,13,214,116,109,159,32,95,226,140,220,57,12
  db 221,31,209,182,143,92,149,184,148,62,113,65,37,27,106,166
  db 3,14,204,72,21,41,56,66,28,193,40,217,25,54,179,117
  db 238,87,240,155,180,170,242,212,191,163,78,218,137,194,175,110
  db 43,119,224,71,122,142,42,160,104,48,247,103,15,11,138,239

align 8

char_table:
  %assign i 0
  %rep 256
    dq CHAR | 1, FIX(i)
    %assign i i + 1
  %endrep

%ifdef FEATURE_LLP64
dcvt: db "%lld", 0
ocvt: db "%llo", 0
xcvt: db "%llx", 0
%else
dcvt: db "%ld", 0
ocvt: db "%lo", 0
xcvt: db "%lx", 0
%endif

gcvt: db "%.16g", 0

rsp_alignment_mask: dq ~(CELLS(2) - 1)
bits_mask: dq BITS_MASK
size_mask: dq SIZE_MASK
byteblock_bit: dq BYTEBLOCK_BIT
closure_type: dq CLOSURE
mark_bit: dq MARK_BIT
special_bit: dq SPECIAL_BIT
align_base: dq ALIGN_BASE
nalign_base: dq ~ALIGN_BASE
pending_signals: dq 0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


section .bss

align 8

toplevel_rsp: resq 1
gc_count: resq 1
buffer: resb 2048
gcsave: resq 2			; holds 2 additional registers to those in "tempregisters"
tempregisters: resq NUMBER_OF_ARGUMENT_REGISTERS - 2		; must follow "gcsave"
locals:	resq NUMBER_OF_NON_REGISTER_ARGUMENTS ; must be right after "tempregisters"!
area1: resb TOTAL_HEAP_SIZE / 2
area2: resb TOTAL_HEAP_SIZE / 2
argv: resq 1
envp: resq 1
saved_ALLOC: resq 1
saved_LIMIT: resq 1
rsp_save: resq 1
stat_buffer: resb 1024

align 8
sigaction_buf:
sigaction_handler: resq 1
%ifdef FEATURE_LINUX
		   resb 152 - CELLS(1)
%elifdef FEATURE_BSD
		   resb 16 - CELLS(1)
%elifdef FEATURE_MAC
                   resb 16 - CELLS(1)
%elifndef FEATURE_WINDOWS
		   resb 16
%endif


section .text


%endif
