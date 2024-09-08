;;
;; Coin yeild percentage settings for SKYLAND Forever
;;

(coins-add 4000)

(map (lambda (kvp) (setvar (cdr kvp) (car kvp)))
 '((40 . "sf_p1_coin_yield")
   (36 . "sf_p2_coin_yield")
   (29 . "sf_p3_coin_yield")
   (22 . "sf_p4_coin_yield")))

(setvar "score_multiplier" 1)
