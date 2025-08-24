;;;
;;; hostile/1/1.lisp
;;;


(opponent-init 6 'hostile)


(flag-show (opponent) flag-id-pirate)


(island-configure
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


(chr-new (opponent) 2 14 'hostile 0)
