;;;
;;; neutral/1/8.lisp
;;;

(eval-file (case (faction)
             ('goblin "/scripts/event/neutral/1/8_goblin.lisp")
             (else    "/scripts/event/neutral/1/8_human.lisp")))
