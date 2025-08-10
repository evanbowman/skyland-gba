;;;
;;; neutral/1/8.lisp
;;;


(cond
  ((equal (faction) 'goblin)
   (eval-file "/scripts/event/neutral/1/8_goblin.lisp"))
  (true
   (eval-file "/scripts/event/neutral/1/8_human.lisp")))
