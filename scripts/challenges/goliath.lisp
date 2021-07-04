;;;
;;; goliath.lisp
;;;


(add-coins (- 0 1000000))
(add-coins 3000)


(terrain (player) 8)
(configure-player
 (player)
 '((power-core 1 13)
   (workshop 1 11)
   (hull 4 14)
   (hull 4 13)
   (hull 4 12)
   (hull 4 11)
   (hull 5 14)
   (hull 5 13)
   (hull 5 12)
   (hull 5 11)
   (hull 6 14)
   (hull 6 13)
   (hull 6 12)
   (hull 6 11)
   (hull 7 14)
   (hull 7 13)))


(add-chr (player) 1 14 'neutral 0)
(add-chr (player) 2 14 'neutral 0)
(add-chr (player) 1 12 'neutral 0)
(add-chr (player) 2 12 'neutral 0)



(init-opponent 10 'hostile)





(configure-player
 (opponent)
 '((power-core 3 10)
   (hull 3 12)
   (hull 4 12)
   (power-core 3 13)
   (power-core 3 8)
   (stairwell 6 11)
   (transporter 6 9)
   (transporter 5 13)
   (stairwell 5 9)
   (cannon 1 14)
   (cannon 1 13)
   (cannon 1 12)
   (cannon 1 11)
   (cannon 1 10)
   (cannon 1 9)
   (hull 2 14)
   (hull 2 13)
   (hull 2 12)
   (hull 2 11)
   (hull 2 10)
   (hull 2 9)
   (workshop 7 13)
   (hull 3 7)
   (hull 4 7)
   (hull 5 8)
   (hull 6 8)
   (hull 7 12)
   (hull 8 12)))


(add-chr (opponent) 3 14 'hostile 0)
