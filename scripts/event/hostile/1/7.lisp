;;;
;;; hostile/1/7.lisp
;;;


(opponent-init 7 'hostile)


(island-configure
 (opponent)
 '((forcefield 0 13)
   (forcefield 0 12)
   (forcefield 0 14)
   (hull 1 10)
   (hull 1 11)
   (arc-gun 1 12)
   (arc-gun 1 13)
   (arc-gun 1 14)
   (missile-silo 2 11)
   (infirmary 2 13)
   (hull 3 10)
   (hull 3 11)
   (hull 3 12)
   (power-core 4 13)
   (power-core 4 11)
   (hull 4 10)
   (hull 5 10)
   (hull 5 9)
   (stairwell 6 11)
   (hull 6 10)
   (hull 6 9)))


(flag-show (opponent) flag-id-pirate)

(chr-new (opponent) 4 14 'hostile 0)
(chr-new (opponent) 5 14 'hostile 0)
