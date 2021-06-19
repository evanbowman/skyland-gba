;;;
;;; hostile_0.lisp
;;;


(eval-other-file (string 'hostile_ 0 '_
                         (if (equal enemies-seen 0)
                             0
                             (+ 1 (cr-choice 6)))
                         '.lisp))

(set 'enemies-seen (+ enemies-seen 1))
