;;;
;;; hostile_0_1.lisp
;;;


(init-opponent 3 'hostile)


(show-flag (opponent))


(configure-player
 (opponent)
 '((cannon 0 13)
   (cannon 0 12)
   (hull 0 14)
   (power-core 1 13)))
