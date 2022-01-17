;;;
;;; easy/score.lisp
;;;


(map (lambda (setvar (cdr $0) (car $0)))
 ;; Percentage of the value
 ;; of the enemy castle
 ;; granted to the player
 ;; after defeating an
 ;; enemy.
 '((58 . "zone1_coin_yield")
   (40 . "zone2_coin_yield")
   (34 . "zone3_coin_yield")
   (28 . "zone4_coin_yield")))
