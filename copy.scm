;;;; copy values into heap-memory
;
; - Note: x86_64 specific


(define (copy x)
  (let ((table '())
	(sh ($inline "mov rax, FIX(CELL_SHIFT)")))
    (let walk ((x x))
      (cond ((%fixnum? x) x)
	    ((assq x table) => cdr)
	    (else
	     (let ((bits (%bits-of x))
		   (size (%size x))
		   (bytes (if (eq? 0 (bitwise-and bits #x10)) ; BYTEBLOCK_BIT
			      (arithmetic-shift size %cell-shift)
			      size))
		   (new (%allocate-block bits bytes #f size #t #f))
		   (start 0))
	       (set! table (cons (cons x new) table))
	       (cond ((eq? bytes size)	; vector-like?
		      (unless (eq? 0 (bitwise-and bits #x20)) ; SPECIAL_BIT
			;; copy 1st slot
			($inline "mov rax, [rax + CELLS(1)]; mov [r11 + CELLS(1)], rax" x new)
			(set! start 1))
		      (do ((i start (%fx+ i 1)))
			  ((%fx>=? i size))
			(%slot-set! new i (walk (%slot-ref x i)))))
		     (else		; string-like
		      ($inline "CALL copy_bytes" (cons x 0) (cons new 0) bytes)))
	       new))))))
