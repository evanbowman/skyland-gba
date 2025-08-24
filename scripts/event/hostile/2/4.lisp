;;;
;;; hostile/2/4.lisp
;;;


;; This one's pretty tough...


(opponent-init 10 'hostile)


(island-configure
 (opponent)
 '((power-core 2 13)
   (power-core 4 13)
   (power-core 1 10)
   (hull 3 11)
   (hull 3 12)
   (hull 1 12)
   (hull 2 12)
   (hull 0 10)
   (forcefield 0 14)
   (forcefield 0 13)
   (cannon 0 12)
   (cannon 0 11)
   (power-core 4 11)
   (stairwell 6 11)
   (infirmary 7 13)
   (stairwell 7 9)
   (transporter 6 9)
   (transporter 5 9)
   (hull 7 8)
   (hull 6 8)
   (hull 5 8)
   (missile-silo 4 9)
   (missile-silo 8 11)
   (missile-silo 9 13)
   (forcefield 8 10)
   (forcefield 9 12)
   (cannon 1 13)
   (cannon 1 14)
   (ion-cannon 2 9)
   (hull 1 9)
   (hull 2 8)))


(flag-show (opponent) flag-id-pirate)


(chr-new (opponent) 2 14 'hostile 0)
(chr-new (opponent) 3 14 'hostile 0)
(chr-new (opponent) 4 14 'hostile 0)
(chr-new (opponent) 5 14 'hostile 0)
(chr-new (opponent) 6 14 'hostile 0)
