;;;
;;; skyland_forever.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(coins-add 3000)
(terrain (player) 4)

(island-configure
 (player)
 '((power-core 1 13)))


(chr-new (player) 1 14 'neutral 0)
(chr-new (player) 2 14 'neutral 0)
(show-flag (player))


(map (lambda (setvar (cdr $0) (car $0)))
 ;; Percentage of the value
 ;; of the enemy castle
 ;; granted to the player
 ;; after defeating an
 ;; enemy.
 '((26 . "sf_p1_coin_yield")
   (18 . "sf_p2_coin_yield")
   (10 . "sf_p3_coin_yield")
   (8  . "sf_p4_coin_yield")))
