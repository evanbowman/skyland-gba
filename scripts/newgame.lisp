;;;
;;; newgame.lisp
;;;
;;; Invoked when starting a new game.
;;;


(setq last-zone 0)

(setq enemies-seen '())
(setq friendlies-seen '())

;; Ids of previously seen quests
(setq qids '())

(setq chr-names '())


(setq quests '())
(setq enemy-count 0)


(flag-show (player))


(coins-add 2500)


(terrain (player) 4)

;; Initial setup for player's island
(island-configure
 (player)
 '((power-core 1 13)))



(chr-new (player) 2 14 'neutral 0)


(eval-file "/scripts/reset_hooks.lisp")
