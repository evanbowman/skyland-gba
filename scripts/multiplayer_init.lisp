;;;
;;; multiplayer_init.lisp
;;;


(eval-other-file "reset_hooks.lisp")




(configure-player
 (player)
 '((power-core 1 13)))


(add-chr (player) 2 14)





(init-opponent 5 'hostile)


(show-flag (opponent))


(configure-player
 (opponent)
 '((power-core 1 13)))


(add-hostile-chr (opponent) 2 14)
