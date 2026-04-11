;;;
;;; surrender/choice.lisp
;;;

(tr-bind-current)

(dialog
 (cond
  ((equal (faction) 'goblin)
   (tr "<c:Goblin Pirates:2>We sssurrender! Honessst, we promise to help you pillage any other cassstles!"))
  (true
   (tr "<c:Goblin Pirates:2>We sssurrender! Honessst, we promise not to pillage any other cassstles!"))))

(defn on-dialog-closed ()
  (setq on-dialog-closed '())
  (let ((c (int (* 72/100 (coins-victory))))
        (score-bonus (int (* 1/4 (coins-victory)))))
    (dialog (tr "The goblins offer surrender, accept terms?"))

    (dialog-opts-reset)
    (dialog-opts-push (format (tr "+1 crew, +%@") c)
                      (lambda ()
                        (coins-add c)
                        (score-add score-bonus)
                        (eval-file "/scripts/event/surrender/crew.lisp")))

  (let ((tot (floor (/ (length (rooms (opponent))) 5))))
      (when tot
        (dialog-opts-push
         (format (tr "Salvage rights: % blocks.") tot)
         (lambda ()
           ((eval-file "/scripts/event/surrender/salvage_rights.lisp") tot)))))


    (dialog-opts-push (format (tr "Unacceptable! (+%@)") (coins-victory)) (lambda ()))))
