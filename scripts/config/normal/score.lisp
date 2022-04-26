;;;
;;; normal/score.lisp
;;;


(map (lambda (syscall "setvar" (cdr $0) (car $0)))
 ;; Percentage of the value
 ;; of the enemy castle
 ;; granted to the player
 ;; after defeating an
 ;; enemy.
 '((40 . "zone1_coin_yield")
   (28 . "zone2_coin_yield")
   (24 . "zone3_coin_yield")
   (20 . "zone4_coin_yield")))

(syscall "setvar" "score_multiplier" 2)
