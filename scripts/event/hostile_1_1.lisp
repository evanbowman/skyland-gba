;;;
;;; hostile_1_1.lisp
;;;


(init-opponent 6 'hostile)


(show-flag (opponent))


(configure-player
 (opponent)
 '((forcefield 0 13)
   (forcefield 0 14)
   (cannon 1 13)
   (cannon 1 14)
   (power-core 2 13)
   (infirmary 4 11)
   (power-core 4 13)
   (missile-silo 3 11)
   (hull 2 11)
   (hull 2 12)
   (forcefield 3 10)
   (hull 5 10)))


(add-chr (opponent) 2 14 'hostile 0)
