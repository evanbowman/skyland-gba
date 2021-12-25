;;;
;;; neutral_1_2.lisp
;;;


(if (choice 2)
    (eval-other-file "/scripts/event/neutral_2_1.lisp")
  (eval-other-file "/scripts/event/neutral_0_2.lisp"))
