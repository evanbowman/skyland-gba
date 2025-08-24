;;;
;;; hostile/0/1.lisp
;;;


(opponent-init 3 'hostile)


(flag-show (opponent) flag-id-pirate)


(island-configure
 (opponent)
 '((cannon 0 13)
   (cannon 0 12)
   (hull 0 14)
   (power-core 1 13)))
