;;;; the WELL1024a random number generator in Scheme
;;
;; /* ***************************************************************************** */
;; /* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
;; /*                 Makoto Matsumoto, Hiroshima University                        */
;; /* Notice:         This code can be used freely for personal, academic,          */
;; /*                 or non-commercial purposes. For commercial purposes,          */
;; /*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
;; /* ***************************************************************************** */
;;
;; I'm not totally sure if this translation is fully correct.


(define-values (InitWELLRNG1024a WELLRNG1024a)
  (let-syntax ((W 32)
	       (R 32)
	       (M1 3)
	       (M2 24)
	       (M3 10))
    (let ((STATE #f)
	  (state_i 0)
	  (z0 0)
	  (z1 0)
	  (z2 0))
      (values
       (lambda (init)
	 (set! state_i 0)
	 (set! STATE (vector-copy init)))
       (let-syntax ((MAT0POS
		     (syntax-rules ()
		       ((_ t v) (bitwise-xor v (arithmetic-shift v (- t))))))
		    (MAT0NEG
		     (syntax-rules ()
		       ((_ t v) (bitwise-xor v (arithmetic-shift v (- t))))))
		    (FACT 2.32830643653869628906e-10)
		    (V0 (vector-ref STATE state_i))
		    (VM1 (vector-ref STATE (bitwise-and (%fx+ state_i M1) #x1f)))
		    (VM2 (vector-ref STATE (bitwise-and (%fx+ state_i M2) #x1f)))
		    (VM3 (vector-ref STATE (bitwise-and (%fx+ state_i M3) #x1f)))
		    (VRm1 (vector-ref STATE (bitwise-and (%fx+ state_i 31) #x1f))))
	 (lambda ()
	   (let ((z0 VRm1)
		 (z1 (bitwise-xor V0 (MAT0POS 8 VM1)))
		 (z2 (bitwise-xor (MAT0NEG -19 VM2) (MAT0NEG -14 VM3))))
	     (vector-set! STATE state_i (bitwise-xor z1 z2))
	     (vector-set! STATE (bitwise-and (%fx+ state_i 31) #x1f) 
			  (bitwise-xor (bitwise-xor (MAT0NEG -11 z0) (MAT0NEG -7 z1)) (MAT0NEG -13 z2)))
	     (set! state_i (bitwise-and (%fx+ state_i 31) #x1f))
	     (* V0 FACT))))))))

#;(InitWELLRNG1024a '#(31542
		     4559
		     10782
		     18486
		     27792
		     18378
		     20672
		     335
		     29029
		     17035
		     259
		     18716
		     28954
		     17406
		     21671
		     855
		     3812
		     26830
		     24562
		     5476
		     9957
		     23525
		     26138
		     11239
		     1607
		     1054
		     11522
		     17156
		     13994
		     6886
		     22006
		     18078))
