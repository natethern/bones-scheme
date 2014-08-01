;;;; compile file with bones running on si

(load "megalet.scm")
(load "match.scm")
(load "pp.scm")
(load "support.scm")
(load "sibones.scm")

(main (cddr (command-line)))
