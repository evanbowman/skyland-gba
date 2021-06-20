;;;
;;; hostile_0_3.lisp
;;;


(init-opponent 5 'hostile)


(show-flag (opponent))


(configure-player
 (opponent)
 '((cannon 0 13)
   (missile-silo 1 13)
   (power-core 2 13)
   (stairwell 4 11)))
