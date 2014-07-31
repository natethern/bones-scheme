;;;; makefile for bones


(require 'sh 'make 'match 'normalize 'apropos)
(run-verbose #t)


(define (all) (bones))

(define (clean)
  (run (rm -f *.o bones bones-x86_64-linux.s si si.s)))

(define compiler-sources
  '("bones.scm"
    "version.scm"
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
    "tsort.scm"
    "uv.scm"
    "ra.scm"
    "cmplr.scm"
    "main.scm"
    "bones.scm"
    "nonstd.scm"
    "base.scm"))

(define compiler-sources-x86_64
  '("x86_64/intrinsics.scm"
    "x86_64.scm"))

(define nasm-format
  (case (system-software)
    ((Linux) 'elf64)
    ((Darwin) 'macho64)
    (else (error "can't determine nasm format for this system"))))

(define gcc
  (case (system-software)
    ((Darwin) "gcc -Wl,-no_pie")
    (else "gcc")))

(define musl-gcc (file-exists? "bin/musl-gcc"))

(define shared-option
  (case (system-software)
    ((Darwin) "-bundle")
    (else "-shared")))

(define target-feature
  (case (system-software)
    ((Linux) 'linux)
    ((Darwin) 'Darwin)
    (else (error "can't determine target format for this system"))))


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

(define (bones-x86_64-mac.s)
  (unless (eq? 'Darwin (system-software))
    (bones))
  (make/proc
   (list (list "bones-x86_64-mac.s"
	       (append compiler-sources compiler-sources-x86_64
		       '("x86_64/mac/syscalls.scm"))
	       (lambda ()
		 (run (./bones bones.scm -o bones-x86_64-mac.s -feature mac)))))))

(define (bones-x86_64-linux.o)
  (bones-x86_64-linux.s)
  (make (("bones-x86_64-linux.o" ("bones-x86_64-linux.s" "x86_64/boneslib.s") 
	  (run (nasm -f elf64 -g -F dwarf bones-x86_64-linux.s -o bones-x86_64-linux.o))))))

(define (bones-x86_64-mac.o)
  (bones-x86_64-mac.s)
  (make (("bones-x86_64-mac.o" ("bones-x86_64-mac.s" "x86_64/boneslib.s") 
	  (run (nasm -f macho64 bones-x86_64-mac.s -o bones-x86_64-mac.o))))))

(define (bones)
  (case (system-software)
    ((Linux)
     (bones-x86_64-linux.o)
     (make (("bones" ("bones-x86_64-linux.o")
             (run (,gcc bones-x86_64-linux.o -o bones))))))
    ((Darwin) 
     (bones-x86_64-mac.o)
     (make (("bones" ("bones-x86_64-mac.o")
             (run (,gcc bones-x86_64-mac.o -o bones))))))))

(define (si)
  (bones)
  (make (("si" ("si.o")
	  (run (,gcc si.o -o si)))
	 ("si.o" ("si.s")	;XXX boneslib?
	  (run (nasm -f ,nasm-format si.s -o si.o)))
	 ("si.s" ("si.scm" "eval.scm" "version.scm" "alexpand.scm") ;XXX intrinsics, etc?
	  (run (./bones si.scm -o si.s))))))

(define (bigbones)
  (bones)
  (make/proc
   (list (list "tmp/bigbones.s"
	       (append compiler-sources compiler-sources-x86_64
		       '("x86_64/linux/syscalls.scm"))
	       (lambda ()
		 (run (./bones bones.scm -feature check -o tmp/bigbones.s -feature linux))))))
  (make (("bigbones" ("tmp/bigbones.o")
	  (run (,gcc tmp/bigbones.o -o bigbones)))
	 ("tmp/bigbones.o" ("bones-x86_64-linux.s" 
			    "x86_64/boneslib.s")
	  (run (nasm -f ,nasm-format -g -F dwarf -DTOTAL_HEAP_SIZE=500_000_000 tmp/bigbones.s
		     -o tmp/bigbones.o))))))

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
    (let* ((name (basename fname))
	   (sname (string-append "tmp/" name ".s"))
	   (oname (string-append "tmp/" name ".o"))
	   (xname (string-append "tmp/" name)))
      (print (padl (string-append " " title) 60 #\=) ": " fname)
      (let ((ok (and (zero? (run* (,cmplr ,(string-append fname ".scm") -o ,sname ,@bopts)))
		     (zero? (run* (nasm -f ,nasm-format ,sname -o ,oname)))
		     (zero? (cond ((memq 'nolibc bopts)
				   (run* (ld ,oname -o ,xname)))
				  ((memq 'glibc bopts)
				   (run* (gcc ,oname -o ,xname)))
				  (else 
				   (run* (,gcc ,oname -o ,xname)))))
		     (zero? (run* (/usr/bin/time ,xname ,@runargs))))))
	(unless ok
	  (print "\n" fname " FAILED.\n"))
	ok))))
	 

(define (check)
  (bones)
  (si)
  (run (mkdir -p tmp))
  (print
   (let ((ok #t))
     (for-each
      (lambda (prg)
	(let* ((bopts (if (member prg '("r4rstest")) '(-case-insensitive) '()))
	       (prg (string-append "tests/" prg)))
	  (unless (compile+run "checked" prg "./bones" '() `(-feature check ,@bopts))
	    (set! ok #f))))
      '("fac" "tak" "mandelbrot" "r4rstest" "r5rs_pitfalls" "dynamic" "compiler" "forth"))
     (for-each
      (lambda (prg)
	(let* ((bopts (if (member prg '("r4rstest")) '(-case-insensitive) '()))
	       (prg (string-append "tests/" prg)))
	  (unless (compile+run "unchecked" prg "./bones" '() bopts)
	    (set! ok #f))))
      '("fac" "tak" "mandelbrot" "r4rstest" "r5rs_pitfalls" "dynamic" "compiler" "forth"))
     (for-each
      (lambda (prg)
	(let* ((bopts (if (member prg '("r4rstest")) '(-case-insensitive) '()))
	       (prg (string-append "tests/" prg)))
	  (unless (compile+run "PIC" prg "./bones" '() `(-feature pic ,@bopts))
	    (set! ok #f))))
      '("fac" "tak" "mandelbrot" "r4rstest" "r5rs_pitfalls" "dynamic" "compiler" "forth"))
     (unless (eq? 'Darwin (system-software))
       (for-each
        (lambda (prg)
          (let ((prg (string-append "tests/" prg)))
            (unless (compile+run "nolibc" prg "./bones" '() '(-feature nolibc))
              (set! ok #f))))
        '("fac" "tak" #;"r4rstest" "dynamic" "forth"))
       (when musl-gcc
	 (fluid-let ((gcc musl-gcc))
	   (for-each
	    (lambda (prg)
	      (let* ((bopts (if (member prg '("r4rstest")) '(-case-insensitive) '()))
		     (prg (string-append "tests/" prg)))
		(unless (compile+run "glibc" prg "./bones" '() `(-feature glibc ,@bopts))
		  (set! ok #f))))
	    '("fac" "tak" "mandelbrot" "r4rstest" "r5rs_pitfalls" "dynamic" "compiler" "forth")))))
     (unless (compile+run "self-compile" "bones" "./bones"
			  `(bones.scm -o tmp/bones.s -feature ,target-feature)
			  `(-feature ,target-feature))
       (set! ok #f))
     (unless (zero? (run* (cmp ,(symbol-append 'bones-x86_64- target-feature '.s) tmp/bones.s)))
       (set! ok #f))
     (print (padl " si" 60 #\=))
     (set! ok (and ok (zero? (run* (./si tests/r4rstest.scm)))))
     (print (padl " embedded" 60 #\=))
     (unless (check-embedded) (set! ok #f))
     (print (padl " grond" 60 #\=))
     (unless (check-grond) (set! ok #f))
     (if ok
	 "\n\nall checks succeeded."
	 "\n\nSOME CHECKS FAILED."))))

(define (check-embedded)
  (bones)
  (let ((r (and (zero? (run* (./bones tests/embedded.scm -o tmp/embedded.s -feature pic -feature embedded)))
		(zero? (run* (nasm -f ,nasm-format tmp/embedded.s -o tmp/embedded1.o -DPREFIX=my)))
		(zero? (run* (nasm -f ,nasm-format tmp/embedded.s -o tmp/embedded2.o -DPREFIX=my_other)))
		(zero? (run* (gcc -g -I. tmp/embedded2.o ,shared-option -o tmp/embedded2.so)))
		(zero? (run* (gcc -g -I. tests/embedded.c tmp/embedded1.o -o tmp/embedded -ldl)))
		(zero? (run* (tmp/embedded))))))
    (unless r
      (print "embedding check failed."))
    r))

(define (check-grond)
  (bigbones)
  (run (mkdir -p tmp))
  (let ((r (and (zero? (run* (memtime ./bigbones tests/grond.scm -feature check -comment 
				      -o tmp/grond.s)))
		(zero? (run* (memtime nasm -f ,nasm-format -g -F dwarf tmp/grond.s -o tmp/grond.o
				      -DTOTAL_HEAP_SIZE=500_000_000
				      -DENABLE_GC_LOGGING)))
		(zero? (run* (,gcc tmp/grond.o -o tmp/grond)))
		(zero? (run* (memtime tmp/grond tests/mandelbrot.scm -ignore-fixnum-overflow -verbose 
				      -clone-size-limit 10))))))
    (unless r
      (print "building or running grond failed."))
    r))

(define (bench)
  (bones)
  (run (echo >>benchmark.txt))
  (run (date +%Y-%m-%d: >>benchmark.txt))
  (run (git rev-parse HEAD >>benchmark.txt))
  (run (echo bones: >>benchmark.txt))
  (run (/usr/bin/time ./bones tests/compiler.scm -o /dev/null >>benchmark.txt 2>&1))
  (run (strip bones ";" ls -l bones >>benchmark.txt))
  (run (echo dynamic: >>benchmark.txt))
  (run (./run tests/dynamic.scm >>benchmark.txt 2>&1))
  (run (strip tests/dynamic ";" ls -l tests/dynamic >>benchmark.txt))
  (run (echo mandelbrot: >>benchmark.txt))
  (run (./run tests/mandelbrot.scm >>benchmark.txt 2>&1))
  (run (strip tests/mandelbrot ";" ls -l tests/mandelbrot >>benchmark.txt))
  (run (echo fft: >>benchmark.txt))
  (run (./run tests/fft.scm >>benchmark.txt 2>&1))
  (run (strip tests/fft ";" ls -l tests/fft >>benchmark.txt))
  (run (echo raytracer: >>benchmark.txt))
  (run (./run tests/raytracer.scm >>benchmark.txt 2>&1))
  (run (strip tests/raytracer ";" ls -l tests/raytracer >>benchmark.txt))
  (run (echo -n "'minimal heap usage: '" >>benchmark.txt))
  (run (./run tests/usedheap.scm >>benchmark.txt))
  (run (echo -n "'large heap '" >>benchmark.txt))
  (run (./run tests/bigheap.scm -DTOTAL_HEAP_SIZE=2_000_000_000 >>benchmark.txt))
  (run (./run -feature nolibc tests/null.scm >>benchmark.txt 2>&1))
  (run (echo -n "'minimal program size: '" >>benchmark.txt))
  (run (strip tests/null ";" ls -l tests/null >>benchmark.txt))
  (print "--------------------------------------------------------------------------------")
  (run (tail -n 60 benchmark.txt)))

(define distfiles
  '("MANUAL.txt"
    "bones-x86_64-linux.s"
    "bones-x86_64-windows.s"
    "bones-x86_64-mac.s"
    "version.scm"
    "alexpand.scm"
    "all.scm"
    "base.scm"
    "bones.scm"
    "cc.scm"
    "uv.scm"
    "ra.scm"
    "cmplr.scm"
    "tsort.scm"
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
    "fastmath.scm"
    "copy.scm"
    "support.scm"
    "si.scm"
    "eval.scm"
    "x86_64/fastmath.scm"
    "x86_64/intrinsics.scm"
    "x86_64/boneslib.s"
    "x86_64/linux/syscalls.scm"
    "x86_64/linux/syscalls-nolibc.scm"
    "x86_64/mac/syscalls.scm"
    "x86_64/windows/syscalls.scm"))

(define (dist)
  (manual)
  (let* ((date (capture (date +%Y-%m-%d)))
	 (arch (string-append "bones-" date)))
    (bones-x86_64-linux.s)
    (bones-x86_64-windows.s)
    (bones-x86_64-mac.s)
    (run (rm -fr ,arch bones.tar.gz bones.zip))
    (run (mkdir -p
		,(string-append arch "/x86_64")
		,(string-append arch "/x86_64/linux")
		,(string-append arch "/x86_64/mac")
		,(string-append arch "/x86_64/windows")))
    (for-each
     (lambda (df)
       (run (cp ,df ,(string-append arch "/" df))))
     distfiles)
    (run (tar cfz bones.tar.gz ,arch))
    (run (zip -r bones.zip ,arch))
    (run (rm -fr ,arch))))

(define (count)
  (run (wc -l ,@compiler-sources)))

(define (manual)
  (make (("MANUAL.html" ("MANUAL.org")
	  (run (emacs --script scripts/makehtml.el)))
	 ("MANUAL.txt" ("MANUAL.org")
	  (run (emacs --script scripts/makeascii.el))))
    '("MANUAL.html" "MANUAL.txt")))

(define (upload)
  (run (upload -d bones MANUAL.html NEWS bones.tar.gz bones.zip)))


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
