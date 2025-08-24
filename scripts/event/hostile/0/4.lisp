;;;
;;; hostile/0/4.lisp
;;;


(opponent-init 6 'hostile)


(flag-show (opponent) flag-id-pirate)


(island-configure
 (opponent)
 '((cannon 1 13)
   (hull 1 14)
   (forcefield 0 13)
   (forcefield 0 14)
   (power-core 2 13)
   (hull 2 12)
   (workshop 4 13)))
