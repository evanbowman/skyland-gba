;;;
;;; surrender.lisp
;;;


(let ((c (choice 3))
      (p "/scripts/event/surrender/"))
  (cond
   ((equal c 0)
    (if (chr-slots (player))
        (eval-file (string p "crew.lisp"))))
   (true
    (if (choice 3)
        (dialog "<c:goblin pirates:2>You pessky humannss! We'll never surrender to the likess of you!")))))
