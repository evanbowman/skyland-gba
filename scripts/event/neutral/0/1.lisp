;; For the sake of variety, the optional fight event has a few variations.

(if (equal (choice 4) 0)
    (load-level "scripts/event/neutral/0/1_1.lisp")
  (load-level "scripts/event/neutral/0/1_0.lisp"))
