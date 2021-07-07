;;;
;;; multiplayer_init.lisp
;;;


(eval-other-file "reset_hooks.lisp")


(add-coins (- 0 999999))
(add-coins 15000)


(terrain (player) 8)


(configure-player
 (player)
 '((power-core 1 13)))


(add-chr (player) 1 14 'neutral 0)
(add-chr (player) 2 14 'neutral 0)





(init-opponent 8 'hostile)


(show-flag (opponent))


(configure-player
 (opponent)
 '((power-core 5 13)))


(add-chr (opponent) 6 14 'hostile 0)
(add-chr (opponent) 5 14 'hostile 0)
