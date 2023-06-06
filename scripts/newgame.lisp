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

(setq adventure-log '())


(flag-show (player) 0)

(coins-set (if (equal (diff) 0) 4000 2500))


(terrain (player) 4)

;; Initial setup for player's island
(island-configure
 (player)
 '((power-core 1 13)))


(if (equal (diff) 0)
    (progn
      (terrain (player) 5)
      (island-configure
       (player)
       '((power-core 1 13)
         (hull 1 12)
         (hull 2 12)
         (hull 3 14)
         (hull 3 13)))))


(setq shop-items '())

;; Variables associated with quests
(setq qvar '())


(chr-new (player) 2 14 'neutral '())


(eval-file "/scripts/reset_hooks.lisp")
