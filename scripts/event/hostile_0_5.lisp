;;;
;;; hostile_0_5.lisp
;;;


(init-opponent 3 'hostile)


(show-flag (opponent))


(configure-player
 (opponent)
 '((cannon 0 12)
   (hull 0 11)
   (missile-silo 1 11)
   (hull 0 13)
   (hull 0 14)
   (power-core 1 13)))
