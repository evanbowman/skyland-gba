;;;
;;; hostile_0_4.lisp
;;;


(init-opponent 3 'hostile)


(show-flag (opponent))


(configure-player
 (opponent)
 '((cannon 0 13)
   (hull 0 14)
   (power-core 1 13)))
