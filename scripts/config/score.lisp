;;;
;;; score.lisp
;;;


(map (lambda (setvar (cdr $0) (car $0)))
 ;; Percentage of the value
 ;; of the enemy castle
 ;; granted to the player
 ;; after defeating an
 ;; enemy.
 '((48 . "zone1_coin_yield")
   (36 . "zone2_coin_yield")
   (25 . "zone3_coin_yield")
   (10 . "zone4_coin_yield")))
