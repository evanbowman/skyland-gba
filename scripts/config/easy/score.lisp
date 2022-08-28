;;;
;;; easy/score.lisp
;;;


(map (lambda (syscall "setvar" (cdr $0) (car $0)))
 ;; Percentage of the value
 ;; of the enemy castle
 ;; granted to the player
 ;; after defeating an
 ;; enemy.
 '((58 . "zone1_coin_yield")
   (50 . "zone2_coin_yield")
   (45 . "zone3_coin_yield")
   (40 . "zone4_coin_yield")))

(syscall "setvar" "score_multiplier" 1)
