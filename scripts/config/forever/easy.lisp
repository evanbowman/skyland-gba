;;
;; Coin yeild percentage settings for SKYLAND Forever
;;

(coins-add 4000)

(map (lambda (syscall "setvar" (cdr $0) (car $0)))
 '((38 . "sf_p1_coin_yield")
   (28 . "sf_p2_coin_yield")
   (20 . "sf_p3_coin_yield")
   (12 . "sf_p4_coin_yield")))
