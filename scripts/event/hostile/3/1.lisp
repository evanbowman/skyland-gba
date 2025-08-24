;;;
;;; hostile/3/1.lisp
;;;


(opponent-init 11 'hostile)


(island-configure
 (opponent)
 '((forcefield 0 10)
   (hull 0 14)
   (forcefield 0 13)
   (forcefield 0 12)
   (forcefield 0 11)
   (forcefield 1 10)
   (forcefield 1 11)
   (hull 1 8)
   (decimator 1 12)
   (hull 1 14)
   (hull 1 9)
   (decimator 2 10)
   (hull 2 9)
   (hull 2 8)
   (energized-hull 2 14)
   (hull 3 12)
   (power-core 3 13)
   (hull 3 8)
   (hull 3 9)
   (stairwell 4 9)
   (hull 4 8)
   (stairwell 5 11)
   (missile-silo 5 9)
   (forcefield 5 8)
   (hull 6 9)
   (reactor 6 12)
   (hull 6 8)
   (power-core 6 10)
   (hull 7 9)
   (transporter 8 13)
   (hull 8 12)
   (transporter 8 10)
   (hull 8 9)
   (power-core 9 13)
   (hull 9 12)
   (missile-silo 9 10)
   (energized-hull 10 12)))


(flag-show (opponent) flag-id-pirate)


(chr-new (opponent) 2 13 'hostile 0)
(chr-new (opponent) 3 11 'hostile 0)
(chr-new (opponent) 6 14 'hostile 0)
(chr-new (opponent) 7 14 'hostile 0)
