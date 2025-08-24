;;;
;;; hostile/0/5.lisp
;;;


(opponent-init 3 'hostile)


(flag-show (opponent) flag-id-pirate)


(island-configure
 (opponent)
 '((cannon 0 12)
   (hull 0 11)
   (missile-silo 1 11)
   (hull 0 13)
   (hull 0 14)
   (power-core 1 13)))
