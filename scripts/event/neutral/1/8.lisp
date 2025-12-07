;;;
;;; neutral/1/8.lisp
;;;


(cond
  ;; TODO...
  ;; ((equal (faction) 'sylph)
  ;;  (eval-file "/scripts/event/neutral/1/8_sylph.lisp"))
  ((equal (faction) 'goblin)
   (eval-file "/scripts/event/neutral/1/8_goblin.lisp"))
  (true
   (eval-file "/scripts/event/neutral/1/8_human.lisp")))
