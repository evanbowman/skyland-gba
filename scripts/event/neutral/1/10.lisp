;;;
;;; neutral/1/10.lisp
;;;


(cond
  ((equal (faction) 'goblin)
   (eval-file "/scripts/event/neutral/1/10_goblin.lisp"))
  (true
   (eval-file "/scripts/event/neutral/1/10_human.lisp")))
