;;;
;;; forever/hard.lisp
;;;
;;; Coin yield percentage settings for SKYLAND Forever
;;;


(coins-add 2500)

(foreach (lambda (kvp) (setvar (second kvp) (first kvp)))
 '((17 . "sf_p1_coin_yield")
   (14 . "sf_p2_coin_yield")
   (7  . "sf_p3_coin_yield")
   (3  . "sf_p4_coin_yield")))

(setvar "score_multiplier" 4)
