;;;
;;; hostile/0/7.lisp
;;;


(opponent-init 6 'hostile)


(flag-show (opponent) flag-id-pirate)


(island-configure
 (opponent)
 '((hull 0 12)
   (cannon 0 13)
   (hull 0 14)
   (hull 1 14)
   (hull 1 13)
   (hull 1 12)
   (missile-silo 2 13)
   (power-core 3 13)
   (hull 3 12)
   (hull 4 12)
   (hull 5 14)
   (hull 5 13)
   (hull 5 12)))
