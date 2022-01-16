;;;
;;; newgame.lisp
;;;
;;; Invoked when starting a new game.
;;;


(eval-file "/scripts/reset_hooks.lisp")

(setq last-zone 0)

(setq enemies-seen '())
(setq friendlies-seen '())

(setq quests '())


(show-flag (player))


(coins-add 2500)


(terrain (player) 4)

;; Initial setup for player's island
(island-configure
 (player)
 '((power-core 1 13)))



(chr-add (player) 2 14 'neutral 0)
