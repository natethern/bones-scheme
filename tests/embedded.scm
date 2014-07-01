;;; test for embedding


(assert (null? (command-line)))

(assert (= 42 (return-to-host 123)))

(return-to-host 43)
