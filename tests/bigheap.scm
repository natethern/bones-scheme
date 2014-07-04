;;;; benchmark GC times for the given heap-size


(define-syntax-rule (time exp)
  (let ((t0 (current-jiffy)))
    exp
    (inexact->exact (round (* 1000 (/ (- (current-jiffy) t0) (jiffies-per-second)))))))

;(print "GC (empty): " (time (reclaim)) " ms")
;(print "free:       " (free))

(define totalsize (quotient ($inline "mov rax, TOTAL_HEAP_SIZE; INT2FIX rax") 2))
(define data (make-vector 10))
(define chunksize (quotient totalsize 10))

;(print "total:      " totalsize)
;(print "chunk size: " chunksize)

(reclaim)

(do ((i 0 (+ i 1)))
    ((>= i 7))
  (vector-set! data i (make-string chunksize)))

;(print "used:      " (* 7 chunksize))
(print "GC (full): " (time (reclaim)) " ms")
;(print "free:      " (free))

(set! data #f)

;(print "GC (empty): " (time (reclaim)) " ms")
;(print "free:       " (free))
