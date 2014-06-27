;;; print amount of heap used in a minimal application


(define total ($inline "mov rax, TOTAL_HEAP_SIZE; INT2FIX rax"))

(reclaim)
(print (- (quotient total 2) (free)))
