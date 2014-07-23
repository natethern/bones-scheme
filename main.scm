;;;; driver


(define (command-line-option? str)
  (let ((len (string-length str)))
    (and (> len 1)
	 (char=? #\- (string-ref str 0)))))

(define (usage)
  (for-each
   (cut display <> (current-error-port))
   '("usage: bones [-v] [-o OUTFILE] [-L LIBRARY_PATH] [-feature FEATURE] [-expand]"
     " [-dump-features] [-comment] [-nostdlib] [-case-insensitive] [-verbose]"
     " [-dump-unused] FILENAME\n"))
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
	(("-v" . _)
	 (print bones-version)
	 (exit))
	(("-o" out . more)
	 (set! opts (cons* 'output-file: out opts))
	 (loop more))
	(("-L" path . more)
	 (set! opts (append (append-map (cut list 'library-path: <>) (string-split path ":")) opts))
	 (loop more))
	(("-case-insensitive" . more)
	 (case-sensitive #f)
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
