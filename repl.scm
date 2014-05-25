;;;; a simple repl for testing things


(define (bones-repl . options)
  (let loop ()
    (print* "bones> ")
    (let ((x (read)))
      (unless (eof-object? x)
	(apply
	 compile 
	 `(begin
	    (program 
	     (include "base.scm")
	     (code
	      (write ,x)
	      (newline))))
	 'output-file: "repl-prg.s"
	 options)
	(when (and (zero? (run* (nasm -f elf64 -g -F dwarf repl-prg.s -o repl-prg.o -DENABLE_GC_LOGGING)))
		   (zero? (run* (bin/musl-gcc repl-prg.o -o repl-prg))))
	  (run* (./repl-prg)))
	(loop))))
  (newline))

