;;;
;;; hostile_0_0.lisp
;;;


(init-opponent 3 'hostile)


(dialog
 "Your castle sails through the clouds, buffeted by a cool breeze. "
 "Another fortress approaches, flying a black flag...")


(show-flag (opponent))


(configure-player
 (opponent)
 (if (cr-choice 2)
     '((cannon 0 13)
       (hull 0 14)
       (power-core 1 13))
   '((hull 2 12)
     (missile-silo 1 11)
     (power-core 1 13))))
