;;;
;;; normal/score.lisp
;;;


(foreach (lambda (kvp) (setvar (second kvp) (first kvp)))
 ;; Percentage of the value
 ;; of the enemy castle
 ;; granted to the player
 ;; after defeating an
 ;; enemy.
 '((40 . "zone1_coin_yield")
   (32 . "zone2_coin_yield")
   (26 . "zone3_coin_yield")
   (20 . "zone4_coin_yield")))

(setvar "score_multiplier" 2)
