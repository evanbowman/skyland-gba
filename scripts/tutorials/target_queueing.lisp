;;;
;;; target_queueing.lisp
;;;


(terrain-set (player) 5)

(island-configure
 (player)
 '((power-core 1 13)
   (hull 1 12)
   (hull 1 11)
   (cannon 2 12)
   (cannon 2 11)
   (hull 2 10)
   (hull 3 14)
   (hull 3 13)))


(opponent-init 4 'hostile)

(island-configure
 (opponent)
 '((hull 0 14)
   (hull 0 13)
   (power-core 0 10)
   (power-core 1 13)
   (hull 1 12)
   (power-core 1 8)
   (hull 2 12)
   (power-core 2 10)))


(groups-add 'Up 2 12)
(groups-add 'Up 2 11)

(autopilot
 '((500 nil)
   (100 "<c:Milo:5>To make target assignment easier, weapons have command queues. <B:0> When picking a weapon target, you can press the SELECT button to create a target queue, allowing you to pick a weapon's (or weapon group's) next three targets all at once! Just like this:")
   (2406 Right)
   (350 Right)
   (767 Up)
   (551 Up)
   (484 Up)
   (1302 A)
   (1920 Select)
   (2187 Down)
   (150 Down)
   (450 Left)
   (701 A)
   (617 Right)
   (384 Up)
   (450 Right)
   (350 Up)
   (484 A)
   (567 Right)
   (417 Down)
   (350 Down)
   (551 Down)
   (400 A)
   (300 Down)
   (9000 B)))
