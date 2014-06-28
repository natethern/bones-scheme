;;;; the most blunt tracing mechanism, ever


(define (fragment exp . depth)
  (let ((depth (optional depth 3)))
    (define (walk x d)
      (if (> d depth)
	  '...
	  (cond ((vector? x) (list->vector (walk (vector->list x) d)))
		((pair? x)
		 (let loop ((x x) (n depth))
		   (cond ((null? x) '())
			 ((zero? n) '(...))
			 ((pair? x) (cons (walk (car x) (+ d 1)) (loop (cdr x) (- n 1))))
			 (else x))))
		(else x))))
    (walk exp 1)))

(define-syntax define
  (let-syntax ((old-define define))
    (syntax-rules ()
      ((_ (name var ...) . body)
       (old-define (name var ...)
		   (old-define (print `(name ,(fragment var) ...)))
		   . body))
      ((_ (name . llist) . body)
       (old-define (name . llist)
		   (old-define (print 'name))
		   . body))
      ((_ name val)
       (old-define name val)))))
