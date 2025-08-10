;;;
;;; neutral/1/0.lisp
;;;


(let ((scenarios
       '("scripts/event/neutral/1/0_goblin.lisp"
         "scripts/event/neutral/1/0_human.lisp")))

  (when (chance 3)
    ;; Sometimes, swap the two scenarios for interesting variability. The goblin
    ;; scenario is still more likely when playing as goblins, but sometimes you
    ;; get the human scenario.
    (setq scenarios (reverse scenarios)))

  (eval-file (get scenarios (if (equal (faction) 'goblin)
                                0
                                1))))
