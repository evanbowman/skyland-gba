;;;
;;; salvage.lisp
;;;


(coins-add 200)


(terrain-set (player) 6)
(island-configure
 (player)
 '((power-core 1 13)
   (workshop 3 13)
   (workshop 3 11)))


(chr-new (player) 1 14 'neutral 0)


(opponent-init 4 'hostile)

(island-configure
 (opponent)
 '((power-core 1 13)
   (missile-silo 0 13)))


(autopilot
 '((100 nil)
   (100 "<c:Milo:5>Hi again! Looks like a hostile castle approaches, let's take a quick look and build some defenses!")
   (1767 L-p)
   (50 L-np)
   (1350 Right)
   (299 Right)
   (283 Right)
   (366 Right)
   (400 Right)
   (450 Right)
   (516 Right)
   (1549 Right)
   (650 Left)
   (316 Left)
   (433 Left)
   (1766 R)
   (1149 Left)
   (650 Left)
   (483 A)
   (1049 A)
   (2333 B)
   (1351 B)
   (100 nil)
   (100 "<c:Milo:5>We need to build some hull, but looks like we don't have enough resources! Not to worry, we can just salvage some rooms to recover some resources. You can access the salvage menu by hovering over a room and pressing B!")
   (2701 Down)
   (466 Down)
   (416 Down)
   (449 Down)
   (833 Up)
   (250 Up)
   (683 B)
   (2065 A)
   (983 Down)
   (616 B)
   (1700 A)
   (1550 R)
   (233 Left)
   (600 Left)
   (766 A)
   (2883 A)
   (416 Right)
   (333 A)
   (350 A)
   (166 Right)
   (716 A)
   (599 Right)
   (266 A)
   (667 A)
   (433 A)
   (817 B)
   (1033 Down)
   (932 A)
   (650 A)
   (416 Down)
   (416 A)
   (683 A)
   (1032 L-p)
   (50 L-np)
   (650 Right)
   (782 Right)
   (8835 Up)))
