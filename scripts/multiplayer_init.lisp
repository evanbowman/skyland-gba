;;;
;;; multiplayer_init.lisp
;;;


(eval-other-file "reset_hooks.lisp")




(configure-player
 (player)
 '((stairwell)
   (power-core 1 13)
   (power-core 3 13)))


(add-chr (player) 2 14 'neutral 0)





(init-opponent 5 'hostile)


(show-flag (opponent))


(configure-player
 (opponent)
 '((stairwell 5 11)
   (power-core 4 13)
   (power-core 2 13)))


(add-chr (opponent) 1 14 'hostile 0)
