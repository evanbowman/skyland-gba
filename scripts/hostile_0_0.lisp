;;;
;;; hostile_0_0.lisp
;;;


(init-opponent 3 'hostile)


(dialog
 "Your castle sails through the clouds, buffeted by a cold wind. "
 "Another fortress approaches, flying a black flag... pirates! ")


(set 'enemies-seen 1)


(configure-player
 (opponent)
 '((cannon 0 13)
   (hull 0 14)
   (power-core 1 13)))
