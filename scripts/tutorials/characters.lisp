(terrain (player) 6)
(configure-player
 (player)
 '((power-core 1 13)
   (workshop 3 13)
   (workshop 3 11)))


(add-chr (player) 1 14 'neutral 0)



(init-opponent 4 'hostile)

(configure-player
 (opponent)
 '((power-core 1 13)
   (missile-silo 0 13)))
