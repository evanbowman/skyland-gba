;;;
;;; neutral/0/3.lisp
;;;


(let ((scenarios
       '("/scripts/event/neutral/0/3_goblin.lisp"
         "/scripts/event/neutral/0/3_human.lisp")))

  (when (equal (choice 3) 0)
    (setq scenarios (reverse scenarios)))

  (eval-file (get scenarios (if (equal (faction) 'goblin)
                                0
                                1))))
