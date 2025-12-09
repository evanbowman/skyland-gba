;;;
;;; neutral/0/5.lisp
;;;


(case (faction)
  ('sylph  (eval-file "/scripts/event/neutral/0/5_sylph.lisp"))
  ('goblin (eval-file "/scripts/event/neutral/0/5_goblin.lisp"))
  (else    (eval-file "/scripts/event/neutral/0/5_human.lisp")))
