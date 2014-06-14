;;;; makefile for bones


(require 'sh 'make 'match 'normalize 'apropos)
(run-verbose #t)


(define (all)
  (bones))

(define (clean)
  (run (rm -f *.o bones bones-x86_64-linux.s)))

(define compiler-sources
  '("bones.scm"
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
    "main.scm"
    "bones.scm"
    "nonstd.scm"
    "base.scm"))

(define compiler-sources-x86_64
  '("x86_64/intrinsics.scm"
    "x86_64.scm"))

(define (bones-x86_64-linux.s)
  (make/proc
   (list (list "bones-x86_64-linux.s"
	       (append compiler-sources compiler-sources-x86_64)
	       (lambda ()
		 (run (./bones1 bones.scm -o bones-x86_64-linux.s -feature linux)))))))

(define (bones-x86_64-windows.s)
  (bones)
  (make/proc
   (list (list "bones-x86_64-windows.s"
	       (append compiler-sources compiler-sources-x86_64)
	       (lambda ()
		 (run (./bones bones.scm -o bones-x86_64-windows.s -feature windows)))))))

(define (bones-x86_64-linux.o)
  (bones-x86_64-linux.s)
  (make (("bones-x86_64-linux.o" ("bones-x86_64-linux.s" "x86_64/boneslib.s" 
				  "x86_64/structured.s")
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
		       (runargs '())
		       (features '()))
    (let* ((name fname)
	   (sname (string-append "tmp/" name ".s"))
	   (oname (string-append "tmp/" name ".o"))
	   (xname (string-append "tmp/" name)))
      (and (zero? (run* (,cmplr ,(string-append fname ".scm") -o ,sname
				,@(append-map (cut list '-feature <>) features))))
	   (zero? (run* (nasm -f elf64 -g -F dwarf ,sname -o ,oname)))
	   (zero? (if (memq features 'linux-bare)
		      (run* (ld ,oname -o ,xname))
		      (run* (bin/musl-gcc ,oname -o ,xname))))
	   (zero? (run* (memtime ,xname ,@runargs)))))))

(define (check)
  (bones)
  (run (mkdir -p tmp))
  (print
   (let ((ok #t))
     (print "---------linux--------------------------------------------------")
     (for-each
      (lambda (prg)
	(let ((copts (if (member prg '("r4rstest")) '("-case-insensitive") '())))
	  (unless (compile+run prg "./bones" '() copts)
	    (set! ok #f))))
      '("fac" "tak" "mandelbrot" "r4rstest" "r5rs_pitfalls" "dynamic" "compiler" "forth"))
     (print "---------linux-bare---------------------------------------------")
     (for-each
      (lambda (prg)
	(unless (compile+run prg "./bones" '() '(linux-bare))
	  (set! ok #f)))
      '("fac" "tak" #;"dynamic" "forth"))
     (print "---------self-compile-------------------------------------------")     
     (unless (compile+run "bones" "./bones" '(bones.scm -o tmp/bones.s))
       (set! ok #f))
     (unless (zero? (run* (cmp bones-x86_64-linux.s tmp/bones.s)))
       (set! ok #f))
     (print "---------embedded-----------------------------------------------")     
     (unless (check-embedded) (set! ok #f))
     (print "----------------------------------------------------------------")     
     (if ok
	 "\nall checks succeeded."
	 "\nsome checks failed."))))

(define (check-embedded)
  (let ((r (and (zero? (run* (./bones embedded.scm -o tmp/embedded.s)))
		(zero? (run* (nasm -f elf64 -g -F dwarf tmp/embedded.s 
				   -o tmp/embedded1.o -DEMBEDDED -DPREFIX=my)))
		(zero? (run* (nasm -f elf64 -g -F dwarf tmp/embedded.s
				   -o tmp/embedded2.o -DEMBEDDED -DPREFIX=my_other)))
		(zero? (run* (gcc -g -I. embedded.c tmp/embedded1.o tmp/embedded2.o -o tmp/embedded)))
		(zero? (run* (tmp/embedded))))))
    (unless r
      (print "embedding check failed."))
    r))

(define (bench)
  (bones)
  (run (echo >>benchmark.txt))
  (run (date +%Y-%m-%d: >>benchmark.txt))
  (run (git rev-parse HEAD >>benchmark.txt))
  (run (echo bones: >>benchmark.txt))
  (run (memtime ./bones compiler.scm -o /dev/null >>benchmark.txt 2>&1))
  (run (echo dynamic: >>benchmark.txt))
  (run (./run dynamic.scm >>benchmark.txt 2>&1))
  (run (echo mandelbrot: >>benchmark.txt))
  (run (./run mandelbrot.scm >>benchmark.txt 2>&1))
  (run (echo fft: >>benchmark.txt))
  (run (./run fft.scm >>benchmark.txt 2>&1))
  (print "--------------------------------------------------------------------------------")
  (run (tail -n 30 benchmark.txt)))

(define distfiles
  '("MANUAL"
    "bones-x86_64-linux.s"
    "bones-x86_64-windows.s"
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
    "nonstd.scm"
    "pp.scm"
    "r5rs.scm"
    "program.scm"
    "source.scm"
    "x86_64/structured.s"
    "x86_64/boneslib.s"
    "support.scm"))

(define (dist)
  (let* ((date (capture (date +%Y-%m-%d)))
	 (arch (string-append "bones-" date)))
    (bones-x86_64-linux.s)
    (bones-x86_64-windows.s)
    (run (rm -fr ,arch))
    (run (mkdir -p ,(string-append arch "/x86_64")))
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
