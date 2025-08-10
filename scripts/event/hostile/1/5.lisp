;;;
;;; hostile/1/5.lisp
;;;


(opponent-init 7 'hostile)


(island-configure
 (opponent)
 '((flak-gun 1 14)
   (flak-gun 1 13)
   (cannon 1 12)
   (hull 1 11)
   (power-core 3 13)
   (power-core 5 13)
   (forcefield 0 14)
   (forcefield 0 13)
   (forcefield 0 12)
   (hull 2 12)
   (hull 3 12)
   (hull 4 12)
   (missile-silo 5 11)
   (hull 6 12)))


(flag-show (opponent) 0)


(chr-new (opponent) 3 14 'hostile 0)
