;;;
;;; hard/score.lisp
;;;


(map (lambda (setvar (cdr $0) (car $0)))
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
