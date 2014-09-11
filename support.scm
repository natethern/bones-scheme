;;;; support-library for bones
;
; - needs match.scm and pp.scm


(define (id x) x)

(define (read-forms file-or-port . reader)
  ((if (string? file-or-port)
       call-with-input-file
       (lambda (fp p) (p fp)))
   file-or-port
   (let ((rd (optional reader read)))
     (lambda (port)
       (let loop ((xs '()))
	 (let ((x (rd port)))
	   (if (eof-object? x)
	       `(begin ,@(reverse xs))
	       (loop (cons x xs)))))))))

(define (emit . xs)
  (for-each display xs))

(define (stringify x)
  (cond ((symbol? x) (symbol->string x))
	((string? x) x)
	((number? x) (number->string x))
	((char? x) (string x))
	(else (error "can't stringify" x))))

(define (symbolify x)
  (cond ((symbol? x) x)
	((string? x) (string->symbol x))
	((char? x) (string->symbol (string x)))
	(else (error "can't symbolify" x))))

(define (listify x)
  (if (list? x) 
      x
      (list x)))

(define (join xs . sep)
  (let ((sep (optional sep ""))
	(out (open-output-string)))
    (let loop ((xs xs))
      (cond ((null? xs) "")
	    ((null? (cdr xs))
	     (display (car xs) out)
	     (get-output-string out))
	    (else 
	     (display (car xs) out)
	     (display sep out)
	     (loop (cdr xs)))))))

(define (every pred lst)
  (let loop ((lst lst))
    (cond ((null? lst))
	  ((not (pred (car lst))) #f)
	  (else (loop (cdr lst))))))

(define (any pred lst)
  (let loop ((lst lst))
    (cond ((null? lst) #f)
	  ((pred (car lst)))
	  (else (loop (cdr lst))))))

(define-syntax inc!
  (syntax-rules ()
    ((_ v) (inc! v 1))
    ((_ v n) 
     (let ((n2 (+ v n)))
       (set! v n2)
       n2))))

(define-syntax dec!
  (syntax-rules ()
    ((_ v) (dec! v 1))
    ((_ v n) 
     (let ((n2 (- v n)))
       (set! v n2)
       n2))))

(define (const c) (lambda _ c))
(define (compl f) (lambda (x) (not (f x))))

(define (o . fs)
  (lambda (x)
    (let loop ((fs fs))
      (if (null? fs)
	  x
	  ((car fs) (loop (cdr fs)))))))

(define (foldl proc init lst)
  (let loop ((lst lst) (r init))
    (if (null? lst) 
	r
	(loop (cdr lst) (proc r (car lst))))))

(define (foldr proc init lst)
  (let loop ((lst lst))
    (if (null? lst)
	init
	(proc (car lst) (loop (cdr lst))))))

(define (flip proc)
  (lambda (x y) (proc y x)))

(define (equal=? x y)
  (let loop ((x x) (y y))
    (cond ((eq? x y))
	  ((number? x) (and (number? y) (= x y)))
	  ((pair? x)
	   (and (pair? y) 
		(loop (car x) (car y))
		(loop (cdr x) (cdr y))))
	  ((vector? x)
	   (and (vector? y)
		(let ((xlen (vector-length x))
		      (ylen (vector-length y)))
		  (and (= xlen ylen)
		       (let loop2 ((i 0))
			 (cond ((>= i xlen) #t)
			       ((loop (vector-ref x i) (vector-ref y i))
				(loop2 (+ i 1)))
			       (else #f)))))))
	  ((string? x)
	   (and (string? y)
		(string=? x y)))
	  (else (eqv? x y)))))		; does not recurse into disjoint types!

(define (alist-cons x y z) (cons (cons x y) z))

(define (make-list n . init)
  (let ((x (optional init #f)))
    (let loop ((n n) (lst '()))
      (if (<= n 0)
	  lst
	  (loop (sub1 n) (cons x lst))))))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (symbol-append . ss)
  (string->symbol
   (apply string-append (map symbol->string ss))))

(define (car+cdr x) (values (car x) (cdr x)))

;; is lst1 a tail of lst2?
(define (tail? lst1 lst2)
  (cond ((null? lst2) (null? lst1))
	((eq? lst1 lst2))
	(else (tail? lst1 (cdr lst2)))))

;; is lst1 a sublist of lst2?
(define (sublist? lst1 lst2)
  (define (follow a b)
    (cond ((null? a))
	  ((null? b) #f)
	  ((equal=? (car a) (car b)) (follow (cdr a) (cdr b)))
	  (else #f)))
  (let loop ((lst2 lst2))
    (cond ((null? lst2) #f)
	  ((follow lst1 lst2))
	  (else (loop (cdr lst2))))))

(define (butlast lst)
  (let loop ((lst lst))
    (if (null? (cdr lst))
	'()
	(cons (car lst) (loop (cdr lst))))))

(define (last lst)
  (let loop ((lst lst))
    (if (null? (cdr lst))
	(car lst)
	(loop (cdr lst)))))

(define (last-pair lst)
  (let loop ((lst lst))
    (if (null? (cdr lst))
	lst
	(loop (cdr lst)))))

(define (filter pred lst)
  (foldr (lambda (x r) (if (pred x) (cons x r) r)) '() lst))

(define (filter-map pred lst)
  (foldr (lambda (x r) 
	   (cond ((pred x) => (lambda (y) (cons y r)))
		 (else r)))
	 '()
	 lst))

(define (append-map proc lst)
  (foldr (lambda (x r) (append (proc x) r)) '() lst))

(define (find pred lst)
  (let loop ((lst lst))
    (cond ((null? lst) #f)
	  ((pred (car lst)) (car lst))
	  (else (loop (cdr lst))))))

(define (find-tail pred ls)
  (let lp ((ls ls))
    (cond ((null? ls) #f)
	  ((pred (car ls)) ls)
	  (else (lp (cdr ls))))))

(define (position pred lst)
  (let loop ((lst lst) (i 0))
    (cond ((null? lst) #f)
	  ((pred (car lst)) i)
	  (else (loop (cdr lst) (+ i 1))))))

(define (posq x lst) 
  (position (cut eq? x <>) lst))

(define (append! . lists)		; SRFI-1 ref. impl.
  ;; First, scan through lists looking for a non-empty one.
  (let lp ((lists lists) (prev '()))
    (if (not (pair? lists)) prev
	(let ((first (car lists))
	      (rest (cdr lists)))
	  (if (not (pair? first)) (lp rest first)
	      ;; Now, do the splicing.
	      (let lp2 ((tail-cons (last-pair first))
			(rest rest))
		(if (pair? rest)
		    (let ((next (car rest))
			  (rest (cdr rest)))
		      (set-cdr! tail-cons next)
		      (lp2 (if (pair? next) (last-pair next) tail-cons)
			   rest))
		    first)))))))

(define (adjoin lst . xs)
  (foldl (lambda (r x) (if (member x r) r (cons x r))) lst xs)) ; uses equal?, not equal=?

(define (difference ls . lss)
  (foldl
   (lambda (ls lst)
     (filter (compl (cut member <> lst)) ls))
   ls
   lss))

(define (union . lss)
  (foldl
   (lambda (ls lst)
     (foldl
      (lambda (ls x)
	(if (any (lambda (y) (equal=? y x)) ls)
	    ls
	    (cons x ls)))
      ls lst))
   '() lss))

(define (intersection ls1 . lss)
  (filter (lambda (x)
	    (every (lambda (lis) (member x lis)) lss)) ; uses equal?, not equal=?
	  ls1))

(define (delete x lst)
  (filter (lambda (y) (not (equal=? x y))) lst))

(define (delete-duplicates lis)
  (let recur ((lis lis))
    (if (null? lis) lis
	(let* ((x (car lis))
	       (tail (cdr lis))
	       (new-tail (recur (delete x tail))))
	  (if (equal=? tail new-tail) lis (cons x new-tail))))))

(define (iota n . start+step)
  (let-optionals start+step ((start 0) (step 1))
    (let loop ((i start) (n n))
      (if (<= n 0) 
	  '()
	  (cons i (loop (add1 i) (sub1 n)))))))

(define (print* . xs)
  (apply emit xs))

(define (show x)
  (pp x)
  x)

(define (concatenate lst) (apply append lst))

(define (interleave lst delim)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (cons
       (car lst)
       (let loop ((lst (cdr lst)))
	 (if (null? lst)
	     '()
	     (cons delim (cons (car lst) (loop (cdr lst)))))))))

(define (split-at i lst)		; allows dotted lists
  (let loop ((lst lst) (i i) (head '()))
    (if (or (<= i 0) (not (pair? lst)))
	(values (reverse head) lst)
	(loop (cdr lst) (sub1 i) (cons (car lst) head)))))

(define (partition pred lst)
  (let loop ((yes '()) (no '()) (lst lst))
    (cond ((null? lst) (values (reverse yes) (reverse no)))
	  ((pred (car lst)) (loop (cons (car lst) yes) no (cdr lst)))
	  (else (loop yes (cons (car lst) no) (cdr lst))))))

(define (span pred lst)		; allows dotted lists and premature end
  (let loop ((lst lst) (head '()))
    (cond ((not (pair? lst)) (values (reverse head) lst))
	  ((pred (car lst)) (loop (cdr lst) (cons (car lst) head)))
	  (else (values (reverse head) lst)))))

(define (take n lst)		; allows dotted lists and premature end
  (let loop ((lst lst) (n n))
    (cond ((or (null? lst) (zero? n)) '())
	  (else (cons (car lst) (loop (cdr lst) (sub1 n)))))))

(define (drop n lst)		; allows dotted lists and premature end
  (let loop ((lst lst) (n n))
    (cond ((or (null? lst) (zero? n)) lst)
	  (else (loop (cdr lst) (sub1 n))))))

(define (cons* first . rest)
  (let recur ((x first) (rest rest))
    (if (pair? rest)
	(cons x (recur (car rest) (cdr rest)))
	x)))

(define (option k args . def)
  (cond ((memq k args) => cadr)
	(else (optional def #f))))

(define (collect-options opt options)
  (let loop ((opts options))
    (cond ((memq opt opts) =>
	   (lambda (p)
	     (cons (cadr p) (loop (cddr p)))))
	  (else '()))))

(define (flatten lst)
  (cond ((null? lst) '())
	((not (pair? lst)) (list lst))
	((null? (car lst)) (cdr lst))
	((pair? (car lst))
	 (append (flatten (car lst)) (flatten (cdr lst))))
	(else (cons (car lst) (flatten (cdr lst))))))

(define (chomp str . chr)
  (let ((chr (optional chr #\newline))
	(len (string-length str)))
    (if (and (positive? len) (char=? chr (string-ref str (- len 1))))
	(substring str 0 (- len 1))
	str)))

(define (read-line . port)
  (let ((port (optional port (current-input-port))))
    (let loop ((lst '()) (cr #f))
      (let ((c (read-char port)))
	(cond ((eof-object? c)
	       (if (null? lst) 
		   c
		   (list->string (reverse lst))))
	      ((char=? #\return c)
	       (loop (if cr (cons c lst) lst) #t))
	      ((char=? #\newline c)
	       (list->string (reverse lst)))
	      (else (loop (cons c (if cr (cons #\return lst) lst)) #f)))))))

(define (read-file in . reader)
  (let ((port (if (string? in) (open-input-file in) in))
	(reader (optional reader read)))
    (let loop ((all '()))
      (match (reader port)
	((? eof-object?)
	 (begin0
	  (reverse all)
	  (when (string? in) (close-input-port port))))
	(x (loop (cons x all)))))))

(define (write-line str . port)
  (let ((port (optional port (current-output-port))))
    (display (chomp str) port)
    (newline port)))

(define (read-all . in)
  (let* ((in (optional in (current-input-port)))
	 (port (if (string? in) (open-input-file in) in))
	 (out (open-output-string)))
    (let loop ()
      (let ((c (read-string 4096 port)))
	(cond ((eof-object? c)
	       (begin0
		 (get-output-string out)
		 (when (string? in) (close-input-port port))))
	      (else
	       (display c out)
	       (loop)))))))

(define (dribble . args)
  (for-each
   (cut display <> (current-error-port))
   args)
  (newline (current-error-port)))

(define (sleep secs)
  (system (string-append "sleep " (number->string secs))))

(define sub1 (cut - <> 1))
(define add1 (cut + <> 1))

(define (numberize x)
  (cond ((number? x) x)
	((boolean? x) (if x 1 0))
	((string? x) (string->number x))
	((symbol? x) (numberize (symbol->string x)))
	((char? x) (numberize (string x)))
	(else (error "can not convert to number" x))))

(define (string-split str . opts)
  (let-optionals opts ((delims " \n\t")
		       (keep #f))
    (let ((len (string-length str))
	  (dlen (string-length delims))
	  (first #f) )
      (define (add from to last)
	(let ((node (cons (substring str from to) '())))
	  (if first
	      (set-cdr! last node)
	      (set! first node) ) 
	  node) )
      (let loop ((i 0) (last #f) (from 0))
	(cond ((>= i len)
	       (when (or (> i from) keep) (add from i last))
	       (or first '()) )
	      (else
	       (let ((c (string-ref str i)))
		 (let scan ((j 0))
		   (cond ((>= j dlen) (loop (+ i 1) last from))
			 ((char=? c (string-ref delims j))
			  (let ((i2 (+ i 1)))
			    (if (or (> i from) keep)
				(loop i2 (add from i last) i2)
				(loop i2 last i2) ) ) )
			 (else (scan (+ j 1))) ) ) ) ) ) ) ) ) )

(define-syntax-rule (push! x v)
  (set! v (cons x v)))

(define-syntax-rule (pop! v)
  (let ((x (car v)))
    (set! v (cdr v))
    x))

(define (delete-file* fn)
  (if (file-exists? fn)
      (begin (delete-file fn) fn)
      #f))

(define (scan str pred . args)
  (let ((len (string-length str)))
    (let-optionals args ((i 0) (step 1))
      (let loop ((i i))
	(cond ((or (negative? i) (>= i len)) #f)
	      ((pred (string-ref str i)) i)
	      (else (loop (+ i step))))))))

(define (eql x) (cut equal=? <> x))

(define (padl str n . fill)
  (let ((fill (optional fill #\space))
	(len (string-length str)))
    (if (> len n)
	str
	(string-append (make-string (- n len) fill) str))))

(define (padr str n . fill)
  (let ((fill (optional fill #\space))
	(len (string-length str)))
    (if (> len n)
	str
	(string-append str (make-string (- n len) fill)))))

(define (pad str n . fill)
  (let ((fill (optional fill #\space))
	(len (string-length str)))
    (if (> len n)
	str
	(let ((m (quotient (- n len) 2)))
	  (string-append 
	   (make-string m fill)
	   str
	   (make-string (- n len m) fill))))))

(define (trim str . delims)
  (let* ((delims (optional delims " \n\t"))
	 (delims (if (string? delims) (string->list delims) delims))
	 (pred (lambda (c) (not (memv c delims))))
	 (len (string-length str))
	 (p1 (or (scan str pred) len))
	 (p2 (or (scan str pred (sub1 len) -1) 0)))
    (if (> p1 p2)
	""
	(substring str p1 (add1 p2)))))

(define (absolute-pathname? pathname)
  (and (positive? (string-length pathname))
       (char=? #\/ (string-ref pathname 0))))

(define (directory? fn)
  (and (file-exists? fn)
       (zero? (system (string-append "test -d " (qs fn))))))

(define (basename str)
  (let ((len (string-length str)))
    (cond ((scan str (cut char=? #\/ <>) (sub1 len) -1) =>
	   (lambda (p) 
	     (if (= p (sub1 len))
		 #f
		 (substring str (add1 p) len))))
	  (else str))))

(define (dirname str)
  (let ((len (string-length str)))
    (cond ((scan str (cut char=? #\/ <>) (sub1 len) -1) => (lambda (p) (substring str 0 p)))
	  (else #f))))

(define (strip-suffix str)
  (let ((len (string-length str)))
    (cond ((scan str (cut char=? #\. <>) (sub1 len) -1) =>
	   (cut substring str 0 <>))
	  (else str))))

(define (replace-suffix suf str)
  (string-append (strip-suffix str) "." suf))

(define (with-input-from-port port thunk)
  (parameterize ((current-input-port port)) (thunk)))

(define (with-output-to-port port thunk)
  (parameterize ((current-output-port port)) (thunk)))

(define (call-with-input-string str proc)
  (let ((in (open-input-string str)))
    (begin0 (proc in) (close-input-port in))))

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (begin0 
     (get-output-string out)
     (close-output-port out))))

(define gentemp
  (let ((counter 0))
    (lambda prefix
      (string-append (optional prefix "T") (number->string (inc! counter))))))

(define (atom? x)
  (or (null? x) (not (pair? x))))

(define (vector-resize vec size . init)
  (let ((new (make-vector size (optional init #f)))
	(from (min (vector-length vec) size)))
    (do ((i 0 (add1 i)))
	((>= i from) new)
      (vector-set! new i (vector-ref vec i)))))

(define (vector-copy! from to . opts)
  (let-optionals opts ((start1 0)
		       (end1 (vector-length from))
		       (start2 0)
		       (end2 (+ start2 (- end1 start1))))
    (let ((count (- end1 start1)))
      (do ((i 0 (add1 i)))
	  ((>= i count))
	(vector-set! to (+ i start2) (vector-ref from (+ start1 i)))))))

(define (copy-list lst) (map id lst))

(define HOME (get-environment-variable "HOME"))

(define (ep path . base)
  (let ((len (string-length path)))
    ;; "base" is ignored when path begins with #\~
    (if (zero? len) 
	base
	(case (string-ref path 0)
	  ((#\~) (string-append HOME (substring path 1 len)))
	  ((#\/) path)		     ;XXX ignores Windows device names
	  (else (string-append (optional base (current-directory)) "/" path))))))

(define (qs str)
  (apply 
   string-append
   "'"
   (append
    (map (lambda (c)
	   (if (char=? #\' c)
	       "'\\''"
	       (string c)))
	 (string->list str))
    (list "'"))))

(define *temporary-files* '())

(define temporary-directory 
  (make-parameter
   (or (get-environment-variable "TMPDIR")
       (get-environment-variable "TMP")
       (get-environment-variable "TEMP")
       "/tmp")))

(define (with-temporary-files thunk)
  (fluid-let ((*temporary-files* *temporary-files*))
    (let ((tmpfiles *temporary-files*))
      (call-with-values thunk
	(lambda results
	  (let loop ((ts *temporary-files*))
	    (if (or (null? ts) (eq? ts tmpfiles))
		(apply values results)
		(begin
		  (delete-file* (car ts))
		  (loop (cdr ts))))))))))

(define make-temporary-filename
  (let ((count 0))
    (lambda args
      (let-optionals args ((prefix "tmp")
			   (extension #f))
	(set! count (+ count 1))
	(string-append
	 (temporary-directory)
	 "/" prefix
	 "." (number->string (current-second))
	 "." (number->string (current-process-id))
	 "." (number->string count)
	 (if extension
	     (string-append "." extension)
	     ""))))))

(define (temporary-file . args)
  (let-optionals args ((prefix "tmp") (suffix #f))
    (let ((tmp (make-temporary-filename prefix suffix)))
      (push! tmp *temporary-files*)
      tmp)))

(define run-verbose (make-parameter #f))
(define run-dry-run (make-parameter #f))

(define (execute cmd)
  (define (build-command cmd)
    (cond ((string? cmd) cmd)
	  ((number? cmd) (number->string cmd))
	  ((char? cmd) (string cmd))
	  ((symbol? cmd) (symbol->string cmd))
	  ((list? cmd) (join (map build-command cmd) " "))
	  (else (error "invalid command part" cmd))))
  (let ((cmd (build-command cmd)))
    (when (run-verbose) 
      (with-output-to-port (current-error-port)
	(cut print "  " cmd)))
    (if (run-dry-run)
	0
	(system cmd))))

(define (check-status s . msg)
  (if (zero? s)
      s 
      (error (optional msg "executing command failed with non-zero exit status") s)))

(define-syntax-rule (run cmd ...)
  (values (check-status (execute `cmd) 'cmd) ...))

(define-syntax-rule (run* cmd ...)
  (values (execute `cmd) ...))

(define-syntax-rule (capture cmd ...)
  (parameterize ((run-verbose #f))
    (with-temporary-files
     (lambda ()
       (values
	(let ((tmp (temporary-file)))
	  (check-status (execute `(cmd > ,(qs tmp))) 'cmd)
	  (trim (with-input-from-file tmp read-all)))
	...)))))

(define-syntax-rule (capture-lines cmd ...)
  (parameterize ((run-verbose #f))
    (with-temporary-files
     (lambda ()
       (values
	(let ((tmp (temporary-file)))
	  (check-status (execute `(cmd > ,(qs tmp))) 'cmd)
	  (read-file tmp read-line))
	...)))))

(define (system-software)
  (let ((s #f))
    (lambda ()
      (or s 
	  (let ((ss (string->symbol (capture (uname)))))
	    (set! s ss)
	    s)))))

(define system-architecture
  (let ((s #f))
    (lambda ()
      (or s 
	  (let ((sa (string->symbol (capture (uname "-m")))))
	    (set! s sa)
	    s)))))

(define (file-executable? fn)
  (zero? (run* (test "-x" ,(qs fn)))))

(define (file-size fn)
  (string->number
   (case (system-software)
     ((Darwin) (capture (stat "-f" "\"%z\"" ,(qs fn))))
     (else (capture (stat "-c" "\"%s\"" ,(qs fn)))))))

(define (file-modification-time fn)
  (string->number
   (case (system-software)
     ((Darwin) (capture (stat "-f" "\"%c\"" ,(qs fn))))
     (else (capture (stat "-c" "\"%Y\"" ,(qs fn)))))))

(define (limited maxdepth exp)
  (define (walk x d)
    (if (> d maxdepth)
	'...
	(cond ((vector? x) (list->vector (walk (vector->list x) d)))
	      ((pair? x)
	       (let loop ((x x) (n maxdepth))
		 (cond ((null? x) '())
		       ((zero? n) '(...))
		       ((pair? x) (cons (walk (car x) (+ d 1)) (loop (cdr x) (- n 1))))
		       (else x))))
	      (else x))))
	(walk exp 1))
