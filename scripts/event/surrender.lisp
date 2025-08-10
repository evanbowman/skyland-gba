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
         "<c:Goblin Pirates:2>You "
         (cond
           ((equal (faction) 'human) "Nasssty Humansss")
           ((equal (faction) 'goblin) "Traitorsss")
           ((equal (faction) 'sylph) "Arrogant Sssylph"))
         "! We'll never surrender to the likesss of you!")))))
