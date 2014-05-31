/*   bones.s - runtime system and core library (mipsel-linux) -*- asm -*-


 * Register usage: 

	$v0, $v1, $a0, $a1: temporary
	$s5: current closure
       $s6: false
       $s7: allocation-limit (fromspace-top - reserve area)
       $s8: allocation-pointer

 * Data layout:

	BLOCK = [header, slot1, ...]
       HEADER = [mark-bit (1), type-bits (7), length (24)]

*/


/********************************************************************************/


.ifndef TOTAL_HEAP_SIZE
 .equ TOTAL_HEAP_SIZE, 100000000
.endif

.equ FROMSPACE_RESERVE, 10000000
.equ MARK_BIT,	0x80000000
.equ SIZE_MASK,       0x00ffffff
.equ BYTEBLOCK_BIT,   0x10000000
.equ SPECIAL_BIT,     0x20000000
.equ HEADER_SHIFT,    24
.equ CELL_SHIFT,      2
.equ ALIGN_BASE,      3
.equ WORD_SIZE,       4

.equ SELF, $s5
.equ ALLOC, $s8
.equ FALSE, $s6
.equ LIMIT, $s7
.equ K, $a2
.equ BASE, $s4

.equ NUMBER_OF_ARGUMENT_REGISTERS, 17

.equ NULL,   0x00000000
.equ SYMBOL, 0x01000000
.equ PAIR,   0x02000000
.equ VECTOR, 0x03000000
.equ CHAR,   0x04000000
.equ EOF,    0x05000000
.equ VOID,   0x06000000
.equ BOOLEAN,0x07000000
.equ PORT,   0x08000000
.equ PROMISE,0x09000000
.equ RECORD, 0x0a000000

.equ FLONUM, 0x10000000
.equ STRING, 0x11000000
.equ CLOSURE,0x20000000


/********************************************************************************/


/* crash */
.macro CRASH
	lw $v0, 0($zero)
.endm


.macro CALL target
	jal \target
.endm


/* save registers before C function call */
.macro SAVE
	addi $sp, $sp, -12 * 4
	sw K, 0($sp)
	sw $a3, 4($sp)
	sw $t0, 8($sp)
	sw $t1, 12($sp)
	sw $t2, 16($sp)
	sw $t3, 20($sp)
	sw $t4, 24($sp)
	sw $t5, 28($sp)
	sw $t6, 32($sp)
	sw $t7, 36($sp)
	sw $t8, 40($sp)
	sw $t9, 44($sp)
.endm


/* restore registers after C function call */
.macro RESTORE
	lw $a2, 0($sp)
	lw $a3, 4($sp)
	lw $t0, 8($sp)
	lw $t1, 12($sp)
	lw $t2, 16($sp)
	lw $t3, 20($sp)
	lw $t4, 24($sp)
	lw $t5, 28($sp)
	lw $t6, 32($sp)
	lw $t7, 36($sp)
	lw $t8, 40($sp)
	lw $t9, 44($sp)
	addiu $sp, $sp, -12 * 4
.endm


/* convert fixnum to 32-bit int */
.macro FIX2INT d, s
	sra \d, \s, 1
.endm


/* convert 32-bit int to fixnum */
.macro INT2FIX d, s
	sll \d, \s, 1
	ori \d, \d, 1
.endm


/* set destination-register to TRUE */
.macro SET_T d
	addiu \d, FALSE, 8
.endm


/* call continuation: K = k, r = result */
.macro CONTINUE r
	move SELF, K
	move K, \r
	lw $v0, 4(SELF)
	li $v1, 1
	jr $v0
.endm


/* allocate flonum: f = flonum -> $v0 */
.macro ALLOC_FLONUM f
	addiu ALLOC, ALLOC, ALIGN_BASE
	li $v0, ~ALIGN_BASE
	and ALLOC, ALLOC, $v0
	s.d \f, 4(ALLOC)
	li $v0, FLONUM | 8
	sw $v0, 0(ALLOC)
	move $v0, ALLOC
	addiu ALLOC, ALLOC, 12
.endm


/* continue with a float-result: K = k, f = float */
.macro CONTINUE_FLOAT f
	ALLOC_FLONUM \f
	CONTINUE $v0
.endm


/* write barrier: d = destination (address in register), s = value (may not be $v0), clobbers $v0 + $a1*/
.macro WRITE_BARRIER d, s
.ifdef DISABLE_WRITE_BARRIER
	sw \s, 0(\d)
.else
	lw $a1, fromspace
	bge $a1, \d, 1f
	lw $a1, fromspace_end
	bgt $a1, \d, 2f
1:	jal write_barrier_trap
2:	sw \s, 0(\d)
.endif
.endm

	      
/* define primitive procedure */
.macro PRIMITIVE name
	.balign 4
\name : 
.endm


/* abort with error message: msg = string */
.macro HALT msg
	.data
1:	.ascii \msg
2:
	.text
	li $v0, 1b
	li $v1, 2b - 1b
	j write_error_and_exit
.endm


/********************************************************************************/


.text

.global main

main:
	SAVE
	sw $a0, argc
	sw $a1, argv
	j init
	      
init:
	sw $zero, gc_count
	li FALSE, false
	lw ALLOC, fromspace
	lw LIMIT, fromspace_end
	li $v1, FROMSPACE_RESERVE
	subu LIMIT, LIMIT, $v1
	li K, terminate_closure
	sw $sp, toplevel_sp
	move SELF, $zero
	j toplevel


terminate:
	lw $sp, toplevel_sp
	lw $v0, exit_code
	FIX2INT $v0, $v0
	RESTORE
	jr $ra


/* consrest: registers/locals = arguments, $v1 = argc, $v0 = non-rest args -> $v0 (ptr) */
consrest:
	li $a1, null
1:	beq $v1, $v0, 3f
	li $a0, NUMBER_OF_ARGUMENT_REGISTERS
	ble $v1, $a0, 2f
	sw $a1, 8(ALLOC)
	li $a1, PAIR | 2
	sw $a1, 0(ALLOC)
	sll $a0, $v1, 2
	li $a1, locals
	addu $a0, $a0, $a1
	move $a1, ALLOC
	lw $a0, 0($a0)
	addiu ALLOC, ALLOC, 12
	addiu $v1, $v1, -1
	j 1b
2:	addiu $v1, $v1, -2
	sll $a0, $v1, 2
        /* XXX assumes "red zone" (verify!) */
	sw $a1, -4($sp)
	li $a1, consrest_jmptable
	addu $a0, $a0, $a1
	lw $a1, -4($sp)
	lw $a0, 0($a0)
	addiu $v1, $v1, 1
	jr $a0
.macro CONSREST1 r
	li $a0, PAIR | 2
	sw $a0, 0(ALLOC)
	sw \r, 4(ALLOC)
	sw $a1, 8(ALLOC)
	move $a1, ALLOC
	addiu ALLOC, ALLOC, 12
	addiu $v1, $v1, 1
	blt $v1, $v0, 3f
.endm
cr_a17:	CONSREST1 $s3
cr_a16:	CONSREST1 $s2
cr_a15:	CONSREST1 $s1
cr_a14:	CONSREST1 $s0
cr_a13:	CONSREST1 $t9
cr_a12:	CONSREST1 $t8
cr_a11:	CONSREST1 $t7
cr_a10:	CONSREST1 $t6
cr_a9:	CONSREST1 $t5
cr_a8:	CONSREST1 $t4
cr_a7:	CONSREST1 $t3
cr_a6:	CONSREST1 $t2
cr_a5:	CONSREST1 $t1
cr_a4:	CONSREST1 $t0
cr_a3:	CONSREST1 $a3
cr_a2:	CONSREST1 $a2
cr_a1:
3:
	move $v0, $v1
	jr $ra

.data

consrest_jmptable:
	.word consrest.a1
	.word consrest.a2
	.word consrest.a3
	.word consrest.a4
	.word consrest.a5
	.word consrest.a6
	.word consrest.a7
	.word consrest.a8
	.word consrest.a9
	.word consrest.a10
	.word consrest.a11
	.word consrest.a12
	.word consrest.a13
	.word consrest.a14
	.word consrest.a15
	.word consrest.a16
	.word consrest.a17

.text


	...
	

;; =: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE numerically_equal
  mov r15, compare_numerically
  jmp pairwise_compare_equal

;; >: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE numerically_greater
  mov r15, compare_numerically
  jmp pairwise_compare_greater

;; <: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE numerically_less
  mov r15, compare_numerically
  jmp pairwise_compare_less

;; >=: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE numerically_greater_or_equal
  mov r15, compare_numerically
  jmp pairwise_compare_greater_or_equal

;; <=: rcx = k, rdx... = numbers, r11 = argc -> (k boolean)
PRIMITIVE numerically_less_or_equal
  mov r15, compare_numerically
  jmp pairwise_compare_less_or_equal


;; pairwise_compare: rcx = k, rdx... = arguments, r11 = argc, r15 = compare -> (k boolean)
;; the compare-function gets 2 arguments in rax + rbx and should set the flags accordingly (and avoid changing any registers but rax)
;; returns #f if argc <= 1
%macro PAIRWISE_COMPARE 2
pairwise_compare_%1:
  cmp r11, 1
  if be
.no:
    CONTINUE FALSE
  endif
  mov rax, rdx			; 1st arg
  mov rbx, rsi			; 2nd arg
  call r15
  j%2 .no
  cmp r11, 4
  je .yes
  mov rax, rbx
  mov rbx, rdi			; 3rd arg
  call r15
  j%2 .no
  cmp r11, 5
  je .yes
  mov rax, rbx
  mov rbx, r8			; 4th arg
  call r15
  j%2 .no
  cmp r11, 6
  je .yes
  mov rax, rbx
  mov rbx, r9			; 5th arg
  call r15
  j%2 .no
  cmp r11, 7
  je .yes
  mov rax, rbx
  mov rbx, r10			; 6th arg
  call r15
  j%2 .no
  cmp r11, 8
  je .yes
  mov rax, rbx
  mov rbx, r12			; 7th arg
  call r15
  j%2 .no
  mov rax, rbx
  sub r11, NUMBER_OF_ARGUMENT_REGISTERS
  mov rdx, locals
  repeat
    test r11, r11
  while nz
    mov rbx, [rdx]		; 7+nth arg
    call r15
    j%2 .no
    dec r11
    add rdx, CELLS(1)
  again
.yes:
  SET_T rax
  CONTINUE rax
%endmacro

PAIRWISE_COMPARE equal, ne
PAIRWISE_COMPARE greater, le
PAIRWISE_COMPARE less, ge
PAIRWISE_COMPARE greater_or_equal, l
PAIRWISE_COMPARE less_or_equal, g


;; comparison functions

compare_numerically:
  test rax, 1
  jz .l1
  test rbx, 1			; rax = fixnum
  jz .l2
  cmp rax, rbx			; rax, rbx = fixnum
  ret
.l1:
  test rbx, 1			; rax = !fixnum
  jz .l3
  ; rax = !fixnum, rbx = fixnum
  FIX2INT rbx
  cvtsi2sd xmm0, rbx
  movsd xmm1, [rax + CELLS(1)]
  ucomisd xmm1, xmm0
  ret  
.l2:
  ; rax = fixnum, rbx = !fixnum
  FIX2INT rax
  cvtsi2sd xmm0, rax
  movsd xmm1, [rbx + CELLS(1)]
  ucomisd xmm0, xmm1
  ret
.l3:
  mov rax, [rax + CELLS(1)]	; rax, rbx = !fixnum
  cmp rax, [rbx + CELLS(1)]
  ret


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
      movsd xmm1, [rdx + CELLS(1)]
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
  CONTINUE rax
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
  cqo
  idiv rbx
  INT2FIX rax
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
  if l
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
  ucomisd xmm0, xmm1
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
  if g
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
  ucomisd xmm0, xmm1
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
  push r11
  cmp r11, NUMBER_OF_ARGUMENT_REGISTERS
  ja .l9
  sub r11, 4
  mov rax, [apply_jmptable + r11 * CELLS(1)]
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
  lea rdi, [tempregisters + r11 * CELLS(1)]
  mov rsi, [rdi]
  mov rdx, null
  pop r11
  sub r11, 2
  repeat
    cmp rsi, rdx
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

apply_jmptable:
  dq apply.l4
  dq apply.l5
  dq apply.l6
  dq apply.l7
  dq apply.l8

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
    mov rdx, ~ALIGN_BASE
    and ALLOC, rdx
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
  mov r11, 1
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
  mov r11, values_continuation
  cmp r11, [rcx + CELLS(1)]
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


/********************************************************************************/


;; called when WRITE_BARRIER detects a write outside of the heap
;;XXX later do something sensible here
write_barrier_trap:
  mov rax, error_msg_1
  mov r11, error_msg_2 - error_msg_1
  call write_error_and_exit


;; called when ALLOC > LIMIT right after a GC (reserve is already subtracted from LIMIT)
heap_full_trap:
  mov rax, error_msg_2
  mov r11, error_msg_3 - error_msg_2
  call write_error_and_exit  


/********************************************************************************/
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
    mov rax, [rsi]		; get header
    mov rcx, rax		; rcx = block size
    mov rdx, SIZE_MASK
    and rcx, rdx
    mov rdx, BYTEBLOCK_BIT
    test rax, rdx
    if nz
      add rcx, CELLS(1)		; binary block, just skip
      add rcx, ALIGN_BASE		; align
      mov rdx, ~ALIGN_BASE
      and rcx, rdx
      add rsi, rcx
    else
      ;; if closure, skip codeptr
      mov rdx, SPECIAL_BIT
      test rax, rdx
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
    mov [rax], rbx ; write forwarding pointer to address olding object-ptr
    ret
  endif
  push rcx
  ;; compute size
  mov rcx, rbx
  mov rdx, SIZE_MASK
  and rcx, rdx
  mov rdx, BYTEBLOCK_BIT
  test rbx, rdx
  if nz
    add rcx, ALIGN_BASE			; align
    shr rcx, CELL_SHIFT			; bytes -> words
  endif
  ;; create forwarding ptr and copy object to tospace
  mov [rdi], rbx		; write header to tospace
  mov rdx, MARK_BIT		; mark header and install forwarding ptr
  or rdx, rdi
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
  mov r15, SIZE_MASK
  and rcx, r15
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


;; fill block with pointers: rax = block, r11 = value -> rax, clobbers r15
fill_slots:
  push rax
  push rcx
  push rdi
  mov rcx, [rax]
  mov r15, SIZE_MASK
  and rcx, r15
  if nz
    lea rdi, [rax + CELLS(1)]
    repeat
      WRITE_BARRIER rdi, r11
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
  mov rbx, [rax + CELLS(1)]
  mov rax, [rax + CELLS(2)]
  FIX2INT rax
  inc rax			; header
  shl rax, CELL_SHIFT
  add rbx, rax
  mov rcx, [r11 + CELLS(1)]
  mov rax, [r11 + CELLS(2)]
  FIX2INT rax
  inc rax			; header
  shl rax, CELL_SHIFT
  add rcx, rax
  test r15, r15
  if nz
    repeat
      mov rdx, [rbx]
      WRITE_BARRIER rcx, rdx
      add rbx, CELLS(1)
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
  fyl2x
  ;; faster than adjusting the rounding mode and using x87 integer store
  fld st0
  fisttp qword [r11]  ; requires SSE3
  fild qword [r11]
  fsub
  f2xm1
  fld1
  fadd
  fild qword [r11]
  fxch
  fscale
  fstp qword [r11]
  fincstp
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
  mov rsi, SIZE_MASK
  and rcx, rsi
  mov rdi, BYTEBLOCK_BIT
  test r15, rdi
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
  mov rsi, SIZE_MASK
  and rcx, rsi
  if z				; zero size?
    SET_T rax
    jmp .done
  endif
  mov rdi, BYTEBLOCK_BIT
  test r15, rdi
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


/********************************************************************************/ hashing function: rax = string -> rax (fixnum, 8-bit hash), clobbers r11, r15
hash_string:
  push rcx
  mov rcx, [rax]
  mov r11, SIZE_MASK
  and rcx, r11
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
    movzx rax, byte [random_numbers + rax]
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
    mov al, [esi - 1]
    cmp al, [edi -1]
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
      mov al, [esi]
      cmp al, 'A'
      if ge			;XXX this can surely be done in a better way
        cmp al, 'Z'
	if le
	  or al, 0x20
	endif
      endif
      mov bl, [edi]
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
      inc esi
      inc edi
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


/********************************************************************************/


%include "x86_64/linux/libcalls.s"


/********************************************************************************/


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

temporary_flonum: dq FLONUM | CELLS(1), 0
flonum_0: dq FLONUM | CELLS(1), __float64__(0.0)
flonum_1: dq FLONUM | CELLS(1), __float64__(1.0)

error_msg_1: db `store to non-heap data detected\n`
error_msg_2: db `out of memory\n`
error_msg_3:

gc_log_format: db `[GC #%d, remaining: %d bytes]\n`, 0

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


/********************************************************************************/


section .bss

toplevel_rsp: resq 1
gc_count: resq 1
buffer: resb 1024
gcsave: resq 2			; holds 2 additional registers to those in "tempregisters"
tempregisters: resq 7		; must follow "gcsave"
locals:	resq 1024		; must be right after "tempregisters"!
area1: resb TOTAL_HEAP_SIZE / 2
area2: resb TOTAL_HEAP_SIZE / 2
argv: resq 1
argc: resq 1
	      
section .text
