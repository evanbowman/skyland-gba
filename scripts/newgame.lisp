;;;
;;; newgame.lisp
;;;
;;; Invoked when starting a new game.
;;;


(eval-other-file "reset_hooks.lisp")

(set 'last-zone 0)

(set 'enemies-seen '())
(set 'friendlies-seen '())



;; Initial setup for player's island
(configure-player
 (player)
 '((power-core 1 13)))


(add-chr (player) 2 14 'neutral 0)

(set 'challenges-completed '())
