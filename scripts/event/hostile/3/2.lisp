;;;
;;; hostile/3/2.lisp
;;;


(opponent-init 13 'hostile)


(island-configure
 (opponent)
 '((forcefield 0 14)
   (forcefield 0 12)
   (forcefield 0 13)
   (hull 1 11)
   (flak-gun 1 14)
   (flak-gun 1 13)
   (flak-gun 1 12)
   (hull 2 10)
   (hull 2 11)
   (power-core 3 11)
   (drone-bay 3 10)
   (hull 3 14)
   (energized-hull 3 13)
   (forcefield 3 9)
   (forcefield 4 9)
   (infirmary 4 13)
   (hull 5 12)
   (transporter 5 10)
   (hull 5 9)
   (stairwell 6 11)
   (hull 6 10)
   (energized-hull 7 14)
   (energized-hull 7 13)
   (drone-bay 7 10)
   (forcefield 7 9)
   (ion-fizzler 7 11)
   (energized-hull 8 11)
   (reactor 8 12)
   (forcefield 8 9)
   (drone-bay 9 9)
   (forcefield 9 8)
   (hull 9 10)
   (energized-hull 9 11)
   (reactor 10 12)
   (hull 10 10)
   (hull 10 11)
   (drone-bay 11 10)
   (forcefield 11 9)
   (hull 11 11)
   (stairwell 12 11)))


(flag-show (opponent) flag-id-pirate)


(chr-new (opponent) 10 14 'hostile 0)
(chr-new (opponent) 11 14 'hostile 0)

(chr-new (opponent) 4 14 'hostile 0)
(chr-new (opponent) 5 14 'hostile 0)
(chr-new (opponent) 6 14 'hostile 0)
