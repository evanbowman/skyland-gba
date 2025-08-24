;;;
;;; hostile/0/2.lisp
;;;


(opponent-init 5 'hostile)


(flag-show (opponent) flag-id-pirate)


(island-configure
 (opponent)
 '((hull 0 13)
   (hull 0 12)
   (hull 0 14)
   (hull 1 12)
   (hull 2 12)
   (forcefield 3 12)
   (power-core 1 13)
   (forcefield 4 12)
   (missile-silo 4 13)
   (missile-silo 3 13)))
