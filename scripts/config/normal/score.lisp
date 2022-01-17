;;;
;;; normal/score.lisp
;;;


(map (lambda (setvar (cdr $0) (car $0)))
 ;; Percentage of the value
 ;; of the enemy castle
 ;; granted to the player
 ;; after defeating an
 ;; enemy.
 '((44 . "zone1_coin_yield")
   (34 . "zone2_coin_yield")
   (27 . "zone3_coin_yield")
   (20 . "zone4_coin_yield")))
