;;;
;;; quest_marker/goblin_pickup_1.lisp
;;;


(dialog "It was a trap!")

(opponent-init 8 'hostile)



(island-configure
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


(flag-show (opponent) flag-id-pirate)


(chr-new (opponent) 4 14 'hostile 0)
(chr-new (opponent) 5 14 'hostile 0)
(chr-new (opponent) 6 14 'hostile 0)
