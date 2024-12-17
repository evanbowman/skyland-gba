;;;
;;; surrender.lisp
;;;


(let ((c (choice 2))
      (p "/scripts/event/surrender/"))
  (cond
   ((equal c 0)
    (eval-file (string p "crew.lisp")))
   ((and (equal c 1) (equal (difficulty) 0))
    (eval-file (string p "crew.lisp")))
   (true
    (if (chance 3)
        (dialog
         "<c:goblin pirates:2>You "
         (cond
           ((equal (faction) 'human) "nassty humannss")
           ((equal (faction) 'goblin) "traitorsss")
           ((equal (faction) 'sylph) "arrogant ssylph"))
         "! We'll never surrender to the likess of you!")))))
