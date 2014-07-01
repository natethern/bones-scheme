;;; export MANUAL as html

(require 'org)

(find-file "MANUAL.org")
(org-export-as-html 3)
