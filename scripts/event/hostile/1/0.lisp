;;;
;;; hostile/1/0.lisp
;;;


(opponent-init 6 'hostile)


(flag-show (opponent) 0)


(island-configure
 (opponent)
 '((forcefield 0 13)
   (forcefield 0 14)
   (cannon 1 13)
   (cannon 1 14)
   (power-core 2 13)
   (infirmary 4 11)
   (power-core 4 13)
   (cannon 3 11)
   (cannon 3 10)
   (hull 3 12)
   (hull 2 12)))


(chr-new (opponent) 4 14 'hostile 0)
