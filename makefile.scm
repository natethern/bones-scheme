;;;; makefile for bones


(require 'sh 'make 'match 'normalize 'apropos)
(run-verbose #t)


(define (all)
  (fac))

(define (clean)
  (run (rm -f fac fac.o fac.s)))

(define (fac.s)
  (make (("fac.s" ("fac.scm" "r5rs.scm" "moresyntax.scm" "intrinsics.scm")
	  (run (./bones1 fac.scm -o fac.s))))))

(define (fac.o)
  (fac.s)
  (make (("fac.o" ("fac.s" "boneslib.s" "structured.s" "libcalls.s")
	  (run (nasm -f elf64 -g -F dwarf fac.s -o fac.o))))))

(define (fac)
  (fac.o)
  (make (("fac" ("fac.o")
	  (run (bin/musl-gcc fac.o -o fac))))))

(define (bones.x.scm)
  (make (("bones.x.scm" ("moresyntax.scm"
			 "intrinsics.scm"
			 "r5rs.scm"
			 "match.scm"
			 "support.scm"
			 "pp.scm"
			 "alexpand.scm"
			 "megalet.scm"
			 "source.scm"
			 "cc.scm"
			 "cps.scm"
			 "mangle.scm"
			 "program.scm"
			 "barebones.scm"
			 "bones.scm")
	  (run (./expand-sources bones.scm bones.x.scm))))))

(define (bones.s)
  (bones.x.scm)
  (make (("bones.s" ("bones.x.scm")
	  (run (./bones1 bones.x.scm -o bones.s))))))

(define (bones.o)
  (bones.s)
  (make (("bones.o" ("bones.s")
	  (run (nasm -f elf64 -g -F dwarf bones.s -o bones.o))))))

(define (bones)
  (bones.o)
  (make (("bones" ("bones.o")
	  (run (bin/musl-gcc bones.o -o bones))))))

(define (backup)
  (let* ((date (capture (date +%Y%m%d)))
	 (name (string-append "bones-" date ".bky.tar.gz")))
    (run (tar cfz ,name .bky))
    (run (scp ,name sem15:))))

(define (tags)
  (make-tags "."))

(define (compile+run fname . opts)
  (let-optionals opts ((cmplr "./bones")
		       (args '()))
    (let* ((name fname)
	   (sname (string-append "tmp/" name ".s"))
	   (oname (string-append "tmp/" name ".o"))
	   (xname (string-append "tmp/" name)))
      (and (zero? (run* (,cmplr ,(string-append fname ".scm") -o ,sname)))
	   (zero? (run* (nasm -f elf64 -g -F dwarf ,sname -o ,oname)))
	   (zero? (run* (bin/musl-gcc ,oname -o ,xname)))
	   (zero? (run* (,xname ,@args)))))))

(define (check)
  (bones)
  (run (mkdir -p tmp))
  (if (and (every compile+run
		  '("fac" "tak" "mandelbrot" "r4test" "r5rs_pitfalls" "dyn" "comp"))
	   (and (compile+run "bones" "./bones" '(bones.x.scm -o tmp/bones.s))
		(and (zero? (run* (cmp bones.s tmp/bones.s))))))
      (print "\nall checks succeeded.")
      (print "\nsome checks failed.")))

(define (-n)
  (run-dry-run #t))

(when (file-exists? "config.scm")
  (load "config.scm"))

(cond-expand
  ((not interactive)
   (for-each 
    (o eval list string->symbol) 
    (let ((args (command-line-arguments)))
      (if (null? args)
	  '("all")
	  args))))
  (else))
