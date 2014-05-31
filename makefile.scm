;;;; makefile for bones


(require 'sh 'make 'match 'normalize 'apropos)
(run-verbose #t)


(define (all)
  (bones))

(define (clean)
  (run (rm -f fac fac.o fac.s)))

(define (bones-x86_64-linux.s)
  (make (("bones-x86_64-linux.s" ("bones.scm"
				  "moresyntax.scm"
				  "x86_64/intrinsics.scm"
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
				  "cmplr.scm"
				  "x86_64.scm"
				  "main.scm"
				  "bones.scm")
	  (run (./bones1 bones.scm -o bones-x86_64-linux.s))))))

(define (bones-x86_64-linux.o)
  (bones-x86_64-linux.s)
  (make (("bones-x86_64-linux.o" ("bones-x86_64-linux.s")
	  (run (nasm -f elf64 -g -F dwarf bones-x86_64-linux.s -o bones-x86_64-linux.o))))))

(define (bones)
  (bones-x86_64-linux.o)
  (make (("bones" ("bones-x86_64-linux.o")
	  (run (bin/musl-gcc bones-x86_64-linux.o -o bones))))))

(define (backup)
  (let* ((date (capture (date +%Y%m%d)))
	 (name (string-append "bones-" date ".git.tar.gz")))
    (run (tar cfz ,name .git))
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
	   (zero? (run* (memtime ,xname ,@args)))))))

(define (check)
  (bones)
  (run (mkdir -p tmp))
  (if (and (every compile+run
		  '("fac" "tak" "mandelbrot" "r4test" "r5rs_pitfalls" "dyn" "comp"))
	   (and (compile+run "bones" "./bones" '(bones.scm -o tmp/bones.s))
		(and (zero? (run* (cmp bones-x86_64-linux.s tmp/bones.s))))))
      (print "\nall checks succeeded.")
      (print "\nsome checks failed.")))

(define (bench)
  (bones)
  (run (echo >>benchmark.txt))
  (run (date +%Y-%m-%d: >>benchmark.txt))
  (run (git rev-parse HEAD >>benchmark.txt))
  (run (echo bones:))
  (run (memtime ./bones comp.scm -o /dev/null 2>>benchmark-txt))
  (run (echo dynamic:))
  (run (./run dyn.scm 2>>benchmark.txt))
  (run (echo mandelbrot:))
  (run (./run mandelbrot.scm 2>>benchmark.txt))
  (run (tail benchmark.txt)))

(define distfiles
  '("README"
    "bones-x86_64-linux.s"
    "alexpand.scm"
    "all.scm"
    "base.scm"
    "bones.scm"
    "cc.scm"
    "cmplr.scm"
    "x86_64.scm"
    "cps.scm"
    "x86_64/intrinsics.scm"
    "mangle.scm"
    "main.scm"
    "match.scm"
    "megalet.scm"
    "moresyntax.scm"
    "nonstd.scm"
    "pp.scm"
    "r5rs.scm"
    "program.scm"
    "source.scm"
    "x86_64/structured.s"
    "x86_64/linux/boneslib.s"
    "x86_64/linux/libcalls.s"
    "support.scm"))

(define (dist)
  (let* ((date (capture (date +%Y-%m-%d)))
	 (arch (string-append "bones-" date)))
    (bones-x86_64-linux.s)
    (run (rm -fr ,arch))
    (run (mkdir -p ,(string-append arch "/x86_64/linux")))
    (for-each
     (lambda (df)
       (run (cp ,df ,(string-append arch "/" df))))
     distfiles)
    (run (tar cfz ,(string-append arch ".tar.gz") ,arch))
    (run (rm -fr ,arch))))

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
