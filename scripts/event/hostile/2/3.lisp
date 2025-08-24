;;;
;;; hostile/2/3.lisp
;;;


(opponent-init 7 'hostile)


(island-configure
 (opponent)
 '((power-core 2 13)
   (power-core 2 11)
   (infirmary 4 13)
   (transporter 6 13)
   (transporter 4 11)
   (ion-cannon 1 14)
   (cannon 0 13)
   (cannon 0 12)
   (cannon 0 11)
   (hull 0 14)
   (hull 1 13)
   (hull 1 12)
   (hull 1 11)
   (power-core 5 11)
   (missile-silo 5 9)
   (missile-silo 6 9)
   (missile-silo 4 9)
   (forcefield 4 8)
   (forcefield 5 8)
   (forcefield 6 8)
   (ion-cannon 3 10)
   (hull 2 10)
   (hull 3 9)))


(flag-show (opponent) flag-id-pirate)


(chr-new (opponent) 2 14 'hostile 0)
(chr-new (opponent) 3 12 'hostile 0)
(chr-new (opponent) 3 14 'hostile 0)
(chr-new (opponent) 2 12 'hostile 0)
