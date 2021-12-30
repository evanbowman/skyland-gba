;;;
;;; hostile/0/6.lisp
;;;


(init-opponent 3 'hostile)


(show-flag (opponent))


(configure-player
 (opponent)
 '((missile-silo 0 13)
   (power-core 1 13)))
