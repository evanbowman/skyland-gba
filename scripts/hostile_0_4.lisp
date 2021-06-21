;;;
;;; hostile_0_4.lisp
;;;


(init-opponent 6 'hostile)


(show-flag (opponent))


(configure-player
 (opponent)
 '((cannon 1 13)
   (hull 1 14)
   (forcefield 0 13)
   (forcefield 0 14)
   (power-core 2 13)
   (hull 2 12)
   (workshop 4 13)))
