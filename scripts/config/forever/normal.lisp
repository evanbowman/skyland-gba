;;;
;;; forever/normal.lisp
;;;
;;; Coin yield percentage settings for SKYLAND Forever
;;;


(coins-add 3000)

(foreach (lambda (kvp) (setvar (second kvp) (first kvp)))
 '((25 . "sf_p1_coin_yield")
   (18 . "sf_p2_coin_yield")
   (10 . "sf_p3_coin_yield")
   (7  . "sf_p4_coin_yield")))

(setvar "score_multiplier" 2)
