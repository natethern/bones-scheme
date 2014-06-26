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
	       (append compiler-sources compiler-sources-x86_64
		       '("x86_64/linux/syscalls.scm"))
	       (lambda ()
		 (run (./bones1 bones.scm -o bones-x86_64-linux.s -feature linux)))))))

(define (bones-x86_64-windows.s)
  (bones)
  (make/proc
   (list (list "bones-x86_64-windows.s"
	       (append compiler-sources compiler-sources-x86_64
		       '("x86_64/windows/syscalls.scm"))
	       (lambda ()
		 (run (./bones bones.scm -o bones-x86_64-windows.s -feature windows)))))))

(define (bones-x86_64-linux.o)
  (bones-x86_64-linux.s)
  (make (("bones-x86_64-linux.o" ("bones-x86_64-linux.s" 
				  "x86_64/boneslib.s" 
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

(define (compile+run title fname . opts)
  (let-optionals opts ((cmplr "./bones")
		       (runargs '())
		       (bopts '()))
    (let* ((name fname)
	   (sname (string-append "tmp/" name ".s"))
	   (oname (string-append "tmp/" name ".o"))
	   (xname (string-append "tmp/" name)))
      (print (padl (string-append " " title) 60 #\=) ": " fname)
      (and (zero? (run* (,cmplr ,(string-append fname ".scm") -o ,sname ,@bopts)))
	   (zero? (run* (nasm -f elf64 -g -F dwarf ,sname -o ,oname)))
	   (zero? (if (memq 'nolibc bopts)
		      (run* (ld ,oname -o ,xname))
		      (run* (bin/musl-gcc ,oname -o ,xname))))
	   (zero? (run* (memtime ,xname ,@runargs)))))))

(define (check)
  (bones)
  (run (mkdir -p tmp))
  (print
   (let ((ok #t))
     (for-each
      (lambda (prg)
	(let ((bopts (if (member prg '("r4rstest")) '(-case-insensitive) '())))
	  (unless (compile+run "linux" prg "./bones" '() bopts)
	    (set! ok #f))))
      '("fac" "tak" "mandelbrot" "r4rstest" "r5rs_pitfalls" "dynamic" "compiler" "forth"))
     (for-each
      (lambda (prg)
	(let ((bopts (if (member prg '("r4rstest")) '(-case-insensitive) '())))
	  (unless (compile+run "linux/PIC" prg "./bones" '() `(-feature pic ,@bopts))
	    (set! ok #f))))
      '("fac" "tak" "mandelbrot" "r4rstest" "r5rs_pitfalls" "dynamic" "compiler" "forth"))
     (for-each
      (lambda (prg)
	(unless (compile+run "linux/nolibc" prg "./bones" '() '(-feature nolibc))
	  (set! ok #f)))
      '("fac" "tak" #;"r4rstest" #;"dynamic" "forth"))
     (unless (compile+run "self-compile" "bones" "./bones"
			  '(bones.scm -o tmp/bones.s -feature linux)
			  '(-feature linux))
       (set! ok #f))
     (unless (zero? (run* (cmp bones-x86_64-linux.s tmp/bones.s)))
       (set! ok #f))
     (print (padl " embedded" 60 #\=))
     (unless (check-embedded) (set! ok #f))
     (if ok
	 "\n\nall checks succeeded."
	 "\n\nSOME CHECKS FAILED."))))

(define (check-embedded)
  (let ((r (and (zero? (run* (./bones embedded.scm -o tmp/embedded.s -feature embedded)))
		(zero? (run* (nasm -f elf64 -g -F dwarf tmp/embedded.s 
				   -o tmp/embedded1.o -DPREFIX=my)))
		(zero? (run* (nasm -f elf64 -g -F dwarf tmp/embedded.s
				   -o tmp/embedded2.o -DPREFIX=my_other)))
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
  (run (strip bones ";" ls -l bones >>benchmark.txt))
  (run (echo dynamic: >>benchmark.txt))
  (run (./run dynamic.scm >>benchmark.txt 2>&1))
  (run (strip dynamic ";" ls -l dynamic >>benchmark.txt))
  (run (echo mandelbrot: >>benchmark.txt))
  (run (./run mandelbrot.scm >>benchmark.txt 2>&1))
  (run (strip mandelbrot ";" ls -l mandelbrot >>benchmark.txt))
  (run (echo fft: >>benchmark.txt))
  (run (./run fft.scm >>benchmark.txt 2>&1))
  (run (strip fft ";" ls -l fft >>benchmark.txt))
  (run (echo -n "'heap used: '" >>benchmark.txt))
  (run (./run freeheap.scm >>benchmark.txt))
  (print "--------------------------------------------------------------------------------")
  (run (tail -n 40 benchmark.txt)))

(define distfiles
  '("MANUAL.txt"
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
    "mangle.scm"
    "main.scm"
    "match.scm"
    "megalet.scm"
    "nonstd.scm"
    "pp.scm"
    "r5rs.scm"
    "program.scm"
    "source.scm"
    "records.scm"
    "copy.scm"
    "x86_64/intrinsics.scm"
    "x86_64/structured.s"
    "x86_64/boneslib.s"
    "x86_64/linux/syscalls.scm"
    "x86_64/linux/syscalls-nolibc.scm"
    "x86_64/windows/syscalls.scm"
    "support.scm"))

(define (dist)
  (let* ((date (capture (date +%Y-%m-%d)))
	 (arch (string-append "bones-" date)))
    (bones-x86_64-linux.s)
    (bones-x86_64-windows.s)
    (run (rm -fr ,arch bones.tar.gz bones.zip))
    (run (mkdir -p
		,(string-append arch "/x86_64")
		,(string-append arch "/x86_64/linux")
		,(string-append arch "/x86_64/windows")))
    (for-each
     (lambda (df)
       (run (cp ,df ,(string-append arch "/" df))))
     distfiles)
    (run (tar cfz bones.tar.gz ,arch))
    (run (zip -r bones.zip ,arch))
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
