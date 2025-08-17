;;;
;;; neutral/1/0.lisp
;;;


(let ((scenarios
       '("scripts/event/neutral/1/0_human.lisp"
         "scripts/event/neutral/1/0_goblin.lisp"
         "scripts/event/neutral/1/0_sylph.lisp")))

  (eval-file (if (chance 3)
                 (get scenarios (choice 3))
                 (format "scripts/event/neutral/1/0_%.lisp" (faction)))))
