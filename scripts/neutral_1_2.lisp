;;;
;;; neutral_1_2.lisp
;;;


(if (cr-choice 2)
    (eval-other-file "neutral_2_1.lisp")
  (eval-other-file "neutral_0_2.lisp"))
