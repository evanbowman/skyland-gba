;;;
;;; neutral_1_2.lisp
;;;


(if (choice 2)
    (eval-other-file "/scripts/neutral_2_1.lisp")
  (eval-other-file "/scripts/neutral_0_2.lisp"))
