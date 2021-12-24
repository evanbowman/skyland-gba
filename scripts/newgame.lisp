;;;
;;; newgame.lisp
;;;
;;; Invoked when starting a new game.
;;;


(eval-other-file "/scripts/reset_hooks.lisp")

(def last-zone 0)

(def enemies-seen '())
(def friendlies-seen '())



(terrain (player) 4)

;; Initial setup for player's island
(configure-player
 (player)
 '((power-core 1 13)))



(add-chr (player) 2 14 'neutral 0)
