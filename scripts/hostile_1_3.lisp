;;;
;;; hostile_1_3.lisp
;;;



(init-opponent 9 'hostile)


(configure-player
 (player)
 '((radar 2 13)
   (power-core 0 13)))

(add-chr (player) 0 14)


(configure-player
 (opponent)
 '((power-core 2 13)
   (power-core 5 13)
   (stairwell 4 11)
   (transporter 5 11)
   (transporter 6 11)
   (infirmary 2 11)
   (hull 1 13)
   (hull 1 14)
   (hull 1 12)
   (hull 1 11)
   (hull 2 10)
   (hull 3 10)
   (hull 4 10)
   (hull 5 10)
   (hull 6 10)
   (cannon 0 14)
   (cannon 0 13)))

(show-flag (opponent))

(add-hostile-chr (opponent) 2 14)
(add-hostile-chr (opponent) 3 14)
(add-hostile-chr (opponent) 5 14)
(add-hostile-chr (opponent) 6 14)
