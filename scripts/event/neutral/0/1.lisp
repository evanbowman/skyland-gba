;; For the sake of variety, the optional fight event has a few variations.

(if (chance 4)
    (eval-file "scripts/event/neutral/0/1_2.lisp")
  (eval-file (format "scripts/event/neutral/0/1_%.lisp" (choice 2))))
