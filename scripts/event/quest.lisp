;;;
;;; quest.lisp
;;;


(gc)

(eval-file
 (string "/scripts/event/quest/" (choice 3) ".lisp"))

(gc)
