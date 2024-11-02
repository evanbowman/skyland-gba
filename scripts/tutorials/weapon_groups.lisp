;;;
;;; weapon_groups.lisp
;;;


(coins-add 5000)

(terrain-set (player) 4)
(island-configure
 (player)
 '((power-core 1 13)
   (cannon 3 13)
   (cannon 3 14)
   (missile-silo 0 13)))


(opponent-init 4 'hostile)

(island-configure
 (opponent)
 '((power-core 1 13)
   (hull 0 13)
   (hull 0 14)
   (hull 0 12)
   (hull 1 12)
   (hull 2 12)))

(flag-show (opponent) 0)


(autopilot
 '((967 Right)
   (216 Right)
   (183 Right)
   (500 nil)
   (100 "<c:Milo:5>Hey again! Did you know that you can assign weapons to groups? Weapon groups allow you to select multiple weapons at once! To edit your weapon groups, hold the START button and press down. Then, you can press the A button to cycle through three groups: the UP group, the LEFT group, and the RIGHT group.")
   (433 Up)
   (250 Up)
   (884 Start-p)
   (400 Down)
   (300 Start-np)
   (1067 Down)
   (633 A)
   (600 Down)
   (567 A)
   (2018 Left)
   (350 Left)
   (366 Left)
   (717 A)
   (633 Up)
   (484 A)
   (667 A)
   (183 B)
   (500 nil)
   (100 "<c:Milo:5>Ok, now, we've assigned our cannons to the UP group, and our missiles to the LEFT group. By holding the START button and pressing the up directional button, we can assign a target for the whole group of cannons! Same thing for the missile group!")
   (350 Right)
   (350 Right)
   (767 Right)
   (283 Up)
   (1084 Start-p)
   (550 Up)
   (266 Start-np)
   (1217 A)
   (1501 Start-p)
   (216 Left)
   (83 Left)
   (183 Start-np)
   (717 Right)
   (466 Right)
   (434 A)
   (3000 nil)
   (100 "<c:Milo:5>By assigning weapon groups to hotkeys, you can quickly reassign a weapon target without needing to scroll to a weapon and click on it! Super useful!")
   (400 nil)
   (100 "<c:Milo:5>Weapon groups have a few different button mappings to make it possible to use them with different GBA button layouts. Hold START during the game for hints.")
   (300 nil)
   (100 "<c:Milo:5>And, finally, you may hold the A button and drag to create a rectangular selection and set the target for batches of weapons. Any weapon targets set by rectangular selection will take priority over a weapon's group. How useful!")
   (1000 nil)))
