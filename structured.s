;;;; "structured" programming, taken mostly from the NASM documentation


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
