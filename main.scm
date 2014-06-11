;;;; driver


(define (command-line-option? str)
  (let ((len (string-length str)))
    (and (> len 1)
	 (char=? #\- (string-ref str 0)))))

(define (usage)
  (display "usage: bones [-o OUTFILE] [-L LIBRARY_PATH] [-feature FEATURE] [-dump] [-expand]" (current-error-port))
  (display " [-dump-source] [-dump-cc] [-dump-nested] [-dump-cps] [-dump-features] [-comment] [-nostdlib] FILENAME\n" (current-error-port))
  (exit 1))

(define (main args)
  (let ((opts '())
	(fname #f))
    (let loop ((args args))
      (match args
	(() 
	 (if fname
	     (apply compile-file fname opts)
	     (usage)))
	(("-o" out . more)
	 (set! opts (cons* 'output-file: out opts))
	 (loop more))
	(("-L" path . more)
	 (set! opts (append (append-map (cut list 'library-path: <>) (string-split path ":")) opts))
	 (loop more))
	(("-feature" f . more)
	 (set! opts (cons* 'feature: (string->symbol f) opts))
	 (loop more))
	(((? command-line-option? opt) . more)
	 (set! opts (cons* (string->symbol (string-append (substring opt 1 (string-length opt)) ":")) #t opts))
	 (loop more))
	((filename . more)
	 (set! fname filename)
	 (loop more))))))
