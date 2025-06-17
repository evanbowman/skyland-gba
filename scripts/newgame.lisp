;;;
;;; newgame.lisp
;;;
;;; Invoked when starting a new game.
;;;


(setq last-zone 0)

(setq enemies-seen nil)
(setq friendlies-seen nil)
(setq surprises-seen nil)

;; Ids of previously seen quests
(setq qids nil)


(setq quests nil)

(setq adventure-log nil)


(flag-show (player) 0)

(coins-set (if (equal (difficulty) 0) 4000 2500))


(terrain-set (player) 4)

;; Initial setup for player's island
(island-configure
 (player)
 '((power-core 1 13)))


(when (equal (difficulty) 0)
  (terrain-set (player) 5)
  (island-configure
   (player)
   '((power-core 1 13)
     (hull 1 12)
     (hull 2 12)
     (hull 3 14)
     (hull 3 13))))


(setq zone-shop-items nil)

;; hint: ((countdown . script) (coutdown . script) ... etc.)
(setq pending-events nil)


;; Variables associated with quests
(setq qvar nil)

(chr-new (player) 2 14 'neutral
         (cond
           ((equal (faction) 'goblin) '((race . 1)))
           ((equal (faction) 'sylph) nil)
           ((equal (faction) 'human) nil)))

(eval-file "/scripts/adventure_vars.lisp")

(eval-file "/scripts/reset_hooks.lisp")
