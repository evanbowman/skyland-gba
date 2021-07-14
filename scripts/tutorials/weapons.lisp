;;;
;;; weapons.lisp
;;;



(terrain (player) 6)
(configure-player
 (player)
 '((power-core 1 13)
   (workshop 1 11)
   (flak-gun 3 14)
   (cannon 4 13)
   (missile-silo 3 12)
   (ion-cannon 4 12)))


(add-chr (player) 1 14 'neutral 0)



(add-coins 20000)


(init-opponent 7 'hostile)

(configure-player
 (opponent)
 '((power-core 3 13)
   (hull 0 14)
   (hull 1 14)
   (hull 2 14)
   (hull 0 13)
   (hull 1 13)
   (hull 2 13)
   (hull 0 12)
   (hull 1 12)
   (hull 2 12)
   (hull 0 11)
   (hull 1 11)
   (hull 2 11)
   (forcefield 3 12)))

(add-chr (opponent) 3 14 'hostile 0)


(autopilot
 '((1134 Right)
   (216 Right)
   (183 Right)
   (133 Right)
   (233 Right)
   (199 Right)
   (800 Left)
   (316 Left)
   (316 A)
   (684 Down)
   (166 Down)
   (416 A)
   (1617 Up)
   (1716 Right)
   (333 Right)
   (166 Right)
   (266 Right)
   (1834 Left)
   (782 Up)
   (966 Left)
   (216 Left)
   (416 Down)
   (316 Left)
   (266 A)
   (568 Down)
   (532 Down)
   (316 A)
   (915 Left)
   (233 Up)
   (1050 A)
   (700 Right)
   (350 A)
   (3583 Right)
   (2833 A)
   (516 Right)
   (200 Right)
   (250 Right)
   (199 Down)
   (367 A)
   (300 Right)
   (300 Right)
   (300 Right)
   (6000 nil)
   (300 Left)
   (300 Left)
   (300 Left)))
