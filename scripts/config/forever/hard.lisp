;;
;; Coin yeild percentage settings for SKYLAND Forever
;;

(coins-add 2500)

(map (lambda (setvar (cdr $0) (car $0)))
 '((18 . "sf_p1_coin_yield")
   (14 . "sf_p2_coin_yield")
   (8 . "sf_p3_coin_yield")
   (4  . "sf_p4_coin_yield")))
