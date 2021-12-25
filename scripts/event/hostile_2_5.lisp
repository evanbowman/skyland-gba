;;;
;;; hostile_2_5.lisp
;;;


(init-opponent 8 'hostile)



(configure-player
 (opponent)
 '((power-core 4 13)
   (power-core 6 13)
   (power-core 6 11)
   (flak-gun 2 14)
   (flak-gun 2 13)
   (flak-gun 2 12)
   (forcefield 1 14)
   (forcefield 1 13)
   (forcefield 1 12)
   (forcefield 0 14)
   (forcefield 0 13)
   (forcefield 0 12)
   (missile-silo 5 11)
   (forcefield 5 10)
   (hull 4 12)
   (hull 2 11)
   (hull 3 11)
   (hull 3 10)
   (hull 6 10)
   (hull 7 10)
   (hull 6 9)
   (hull 7 9)))


(show-flag (opponent))


(add-chr (opponent) 4 14 'hostile 0)
(add-chr (opponent) 5 14 'hostile 0)
(add-chr (opponent) 6 14 'hostile 0)
