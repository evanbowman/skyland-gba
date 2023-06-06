;;;
;;; surrender.lisp
;;;


(let ((c (choice 2))
      (p "/scripts/event/surrender/"))
  (cond
   ((equal c 0)
    (eval-file (string p "crew.lisp")))
   ((and (equal c 1) (equal (diff) 0))
    (eval-file (string p "crew.lisp")))
   (true
    (if (equal 0 (choice 3))
        (dialog "<c:goblin pirates:2>You pessky humannss! We'll never surrender to the likess of you!")))))
