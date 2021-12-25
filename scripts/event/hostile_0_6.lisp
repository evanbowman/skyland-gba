;;;
;;; hostile_0_6.lisp
;;;


(init-opponent 3 'hostile)


(show-flag (opponent))


(configure-player
 (opponent)
 '((missile-silo 0 13)
   (power-core 1 13)))
