;;;; program.scm - expand (extended) SRFI-7 specification and construct cond-expand definition


; Syntax:
;
; PROGRAM = (program CLAUSE ...)
;
; CLAUSE = (requires REQ ...)                      tests whether features are available
;        | (files FILE ...)                        include files
;        | (provide ID ...)                        define features visible to later clauses
;        | (code EXPR ...)                         include code
;        | (include FILE ...)                      include other program clauses
;        | (feature-cond (REQ CLAUSE ...) ... [(else CLAUSE ...)])   process clauses depending on available features
;        | (cond-expand (REQ CLAUSE ...) ... [(else CLAUSE ...)])   alias for "feature-cond"
;        | (error STRING)                          signal error and abort
;
; REQ = ID
;     | (and ID ...)
;     | (or ID ...)
;     | (not ID)
;
; FILE = STRING | SYMBOL | (FILE ...)


;;; holds list of symbols that specify features of the implementation or target platform

(define implementation-features '())
(define file-search-path '("."))


;;; Expand SRFI-7 program specification

(define (program-file-filename x)
  (cond ((string? x) x)
	((symbol? x) (symbol->string x))
	((list? x) (join (map program-file-filename x) "/"))
	((not (pair? x)) (error "invalid filename" x))
	(else (stringify x))))

(define (expand-program prg . src)
  (let* ((src (optional src #f))
	 (spath (if src
		    (cons src file-search-path)
		    file-search-path)))
    (define (find-file name)
      (let loop ((dirs spath))
	(and (pair? dirs)
	     (let ((fname (string-append (car dirs) "/" name)))
	       (if (file-exists? fname)
		   fname
		   (loop (cdr dirs)))))))
    (define (localize path)
      (let ((path (program-file-filename path)))
	(cond ((and (positive? (string-length path))
		    (memq (string-ref path 0) '(#\\ #\/)))
	       path)
	      ((find-file path))
	      (else (error "file not found" path)))))
    (define expand-req
      (match-lambda
	(('and reqs ...)
	 (every expand-req reqs))
	(('or reqs ...)
	 (any expand-req reqs))
	(('not req) 
	 (not (expand-req req)))
	((? symbol? r)
	 (memq r implementation-features))
	(r (error "invalid feature requirement" r))))
    (define (expand-clause clause)
      (match clause
	(('requires ids ...)
	 (for-each
	  (lambda (id)
	    (unless (memq id implementation-features)
	      (error "required feature not available" id)))
	  ids)
	 '(void))
	(('files fns ...)
	 `(begin
	    ,@(map (lambda (fn) (read-forms (localize fn) read)) fns)))
	(('code exps ...)
	 `(begin ,@exps))
	(('provide ids ...)
	 (set! implementation-features (append ids implementation-features))
	 '(begin #t))
	(('error msg)
	 (error msg))
	(('include fns ...)
	 `(begin
	    ,@(map (lambda (fn)
		     `(begin ,@(map expand-clause (read-file (localize fn)))))
		   fns)))
	(((or 'cond-expand 'feature-cond) clauses ...)
	 (let loop ((cs clauses))
	   (match cs
	     (() (error "no requirement satisfied" (map car clauses)))
	     ((('else clauses ...))
	      `(begin ,@(map expand-clause clauses)))
	     (((req clauses ...) . more)
	      (if (expand-req req)
		  `(begin ,@(map expand-clause clauses))
		  (loop more)))
	     (c (error "invalid clause syntax" c)))))
	(_ (error "invalid program clause" clause))))
    (set! src (and src (string? src) (dirname src)))
    (match prg
      (('program clauses ...)
       `(begin ,@(map expand-clause clauses)))
      (_ (error "invalid program form" prg)))))


;;; create syntax-definition for "cond-expand" from available feature set

(define (generate-cond-expand features)
  `(define-syntax cond-expand
     (syntax-rules (and or not else ,@features)
       ((cond-expand) (error 'cond-expand "no matching clause"))
       ,@(map (lambda (f)
		`((cond-expand (,f body ...) . more-clauses)
		  (begin body ...)))
	      features)
       ((cond-expand (else body ...)) (begin body ...))
       ((cond-expand ((and) body ...) more-clauses ...) (begin body ...))
       ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
	(cond-expand
	  (req1 (cond-expand
		  ((and req2 ...) body ...)
		  more-clauses ...))
	  more-clauses ...))
       ((cond-expand ((or) body ...) more-clauses ...) 
	(cond-expand more-clauses ...))
       ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
	(cond-expand
	  (req1 (begin body ...))
	  (else
	   (cond-expand
	     ((or req2 ...) body ...)
	     more-clauses ...))))
       ((cond-expand ((not req) body ...) more-clauses ...)
	(cond-expand
	  (req (cond-expand more-clauses ...))
	  (else body ...)))
       ((cond-expand (feature-id body ...) more-clauses ...)
	(cond-expand more-clauses ...)))))
