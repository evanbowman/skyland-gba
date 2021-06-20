;;;
;;; hostile_0.lisp
;;;


(eval-other-file "reset_hooks.lisp")



(eval-other-file (string 'hostile_ 0 '_
                         (if (equal enemies-seen 0)
                             0
                             (+ 1 (cr-choice 7)))
                         '.lisp))

(set 'enemies-seen (+ enemies-seen 1))
