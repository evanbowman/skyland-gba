;;;
;;; newgame.lisp
;;;
;;; Invoked when starting a new game.
;;;


(eval-other-file "/scripts/reset_hooks.lisp")

(setq last-zone 0)

(setq enemies-seen '())
(setq friendlies-seen '())


(add-coins 2500)


(terrain (player) 4)

;; Initial setup for player's island
(configure-player
 (player)
 '((power-core 1 13)))



(add-chr (player) 2 14 'neutral 0)
