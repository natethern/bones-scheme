;;; Topological sort with cycle detection:
;;
;; A functional implementation of the algorithm described in Cormen,
;; et al. (2009), Introduction to Algorithms (3rd ed.), pp. 612-615.

(define (topological-sort dag pred)
  (define (alist-ref x al def)
    (cond ((find (lambda (a) (pred x (car a))) al) => cdr)
	  (else def)))
  (define (alist-update! x y al)
    (cond ((find (lambda (a) (pred x (car a))) al) =>
	   (lambda (a)
	     (set-cdr! a y)
	     al))
	  (else (cons (cons x y) al))))
  (define (visit dag node edges path state)
    (case (alist-ref node (car state) #f)
      ((grey)
       (error "cycle detected in topological sort" node (reverse path) dag))
      ((black)
       state)
      (else
       (let walk ((edges (or edges (alist-ref node dag '())))
                  (state (cons (cons (cons node 'grey) (car state))
                               (cdr state))))
         (if (null? edges)
             (cons (alist-update! node 'black (car state))
                   (cons node (cdr state)))
             (let ((edge (car edges)))
               (walk (cdr edges)
                     (visit dag
                            edge
                            #f
                            (cons edge path)
                            state))))))))
  (let loop ((dag dag)
             (state (cons (list) (list))))
    (if (null? dag)
        (cdr state)
        (loop (cdr dag)
              (visit dag
                     (caar dag)
                     (cdar dag)
                     '()
                     state)))))
