;;;
;;; easy/score.lisp
;;;


(map (lambda (setvar (cdr $0) (car $0)))
 ;; Percentage of the value
 ;; of the enemy castle
 ;; granted to the player
 ;; after defeating an
 ;; enemy.
 '((72 . "zone1_coin_yield")
   (65 . "zone2_coin_yield")
   (55 . "zone3_coin_yield")
   (45 . "zone4_coin_yield")))

(setvar "score_multiplier" 1)
