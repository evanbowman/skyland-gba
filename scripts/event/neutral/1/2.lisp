;;;
;;; neutral/1/2.lisp
;;;


(if (choice 2)
    (eval-other-file "/scripts/event/neutral/2/1.lisp")
  (eval-other-file "/scripts/event/neutral/0/2.lisp"))
