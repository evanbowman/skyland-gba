;;;
;;; neutral/0/3.lisp
;;;


(let ((scenarios '()))
  (cond
    ;; Sylph can encounter their own military depot or either merchant type
    ((equal (faction) 'sylph)
     (setq scenarios
       (if (chance 2) ;; 50% chance for Sylph military depot
           '("/scripts/event/neutral/0/3_sylph.lisp")
           '("/scripts/event/neutral/0/3_goblin.lisp"
             "/scripts/event/neutral/0/3_human.lisp"))))

    ;; Goblins mostly encounter other goblins, rarely humans
    ((equal (faction) 'goblin)
     (setq scenarios '("/scripts/event/neutral/0/3_goblin.lisp"
                       "/scripts/event/neutral/0/3_human.lisp"))
     (when (chance 4) ;; Rarely swap to human
       (setq scenarios (reverse scenarios))))

    ;; Humans encounter both, with some randomization
    (true
     (setq scenarios '("/scripts/event/neutral/0/3_goblin.lisp"
                       "/scripts/event/neutral/0/3_human.lisp"))
     (when (chance 2)
       (setq scenarios (reverse scenarios)))))

  ;; Execute the selected scenario
  (eval-file (car scenarios)))
