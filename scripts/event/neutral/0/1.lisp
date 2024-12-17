;; For the sake of variety, the optional fight event has a few variations.

(if (chance 4)
    (eval-file "scripts/event/neutral/0/1_1.lisp")
  (eval-file "scripts/event/neutral/0/1_0.lisp"))
