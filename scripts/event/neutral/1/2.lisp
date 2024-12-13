;;;
;;; neutral/1/2.lisp
;;;


(if (choice 2)
    (eval-file "/scripts/event/neutral/2/1_alt0.lisp")
  (eval-file "/scripts/event/neutral/0/2.lisp"))
