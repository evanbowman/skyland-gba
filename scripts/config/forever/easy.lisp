;;;
;;; forever/easy.lisp
;;;
;;; Coin yield percentage settings for SKYLAND Forever
;;;


(coins-add 4000)

(foreach (lambda (kvp) (setvar (second kvp) (first kvp)))
 '((40 . "sf_p1_coin_yield")
   (36 . "sf_p2_coin_yield")
   (29 . "sf_p3_coin_yield")
   (22 . "sf_p4_coin_yield")))

(setvar "score_multiplier" 1)
