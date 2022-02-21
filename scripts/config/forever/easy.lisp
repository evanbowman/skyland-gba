;;
;; Coin yeild percentage settings for SKYLAND Forever
;;

(coins-add 4000)

(map (lambda (syscall "setvar" (cdr $0) (car $0)))
 '((40 . "sf_p1_coin_yield")
   (32 . "sf_p2_coin_yield")
   (26 . "sf_p3_coin_yield")
   (18 . "sf_p4_coin_yield")))

(syscall "setvar" "score_multiplier" 1)
