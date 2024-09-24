;;;
;;; hard/score.lisp
;;;


(foreach (lambda (kvp) (setvar (second kvp) (first kvp)))
 ;; Percentage of the value
 ;; of the enemy castle
 ;; granted to the player
 ;; after defeating an
 ;; enemy.
 '((28 . "zone1_coin_yield")
   (22 . "zone2_coin_yield")
   (16 . "zone3_coin_yield")
   (10 . "zone4_coin_yield")))

(setvar "score_multiplier" 4)
