;;;
;;; surrender.lisp
;;;

(tr-bind-current)

(let ((c (choice 2))
      (p "/scripts/event/surrender/"))
  (cond
   ((equal c 0)
    (eval-file (string p "choice.lisp")))
   ((and (equal c 1) (equal (difficulty) difficulty-beginner))
    (eval-file (string p "choice.lisp")))
   (true
    (if (chance 3)
        (dialog
         (format (tr "<c:Goblin Pirates:2>You %! We'll never surrender to the likesss of you!")
                 (tr (case (faction)
                       ('human "Nasssty Humansss")
                       ('goblin "Traitorsss")
                       ('sylph "Arrogant Sssylph")))))))))
