;;;
;;; hostile_0_0.lisp
;;;


(init-opponent 3 'hostile)


(dialog
 "Your castle sails through the clouds, buffeted by a cold wind. "
 "Far on the horizon, another fortress approaches, flying a black flag... pirates! ")


(set 'enemies-seen 1)


(configure-player
 (opponent)
 '((cannon 0 14)
   (hull 0 13)
   (power-core 1 13)))
