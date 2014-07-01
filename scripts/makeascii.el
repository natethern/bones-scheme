;;; export MANUAL as text

(require 'org)

(find-file "MANUAL.org")
(org-export-as-ascii 3)
