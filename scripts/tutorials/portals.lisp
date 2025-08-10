;;;
;;; portals.lisp
;;;


(terrain-set (player) 9)

(island-configure
 (player)
 '((infirmary 0 13) (portal 2 14) (power-core 3 13) (workshop 3 11) (portal 3 10) (reactor 4 8) (portal 5 12) (manufactory 5 13) (hull 6 12) (missile-silo 6 10) (portal 7 12) (hull 7 11) (reactor 7 8) (stairwell 8 11)))

(opponent-init 7 'hostile)
(island-configure (opponent) '((power-core 1 13) (infirmary 5 13)))

(chr-new (player) 7 10 'neutral '((id . 2)))
(chr-new (player) 4 10 'neutral '((id . 1)))
(chr-new (opponent) 2 14 'hostile '((id . 3)))
(chr-new (opponent) 1 14 'hostile '((id . 4)))

(coins-set 500000)

(defn on-fadein () (sel-move (player) 0 14))

(autopilot
 '((2000 nil)
   (100 "<c:Milo:5>Sometimes, you may want to connect disjoint sections of your castle... <B:0> Portals allow you to link areas of your fortress together, allowing your crew to travel quickly between them!")
   (550 Select)
   (200 Select)
   (550 Right)
   (183 Right)
   (317 Right)
   (483 Right)
   (617 Up)
   (217 Up)
   (283 Up)
   (233 Up)
   (734 A)
   (183 Down)
   (166 Down)
   (183 Down)
   (167 Down)
   (150 A)
   (2538 A)
   (217 Up)
   (166 Up)
   (217 A)
   (2000 nil)
   (100 "<c:Milo:5>Just be careful not to let portals catch on fire! Your crewmembers aren't the only thing that can pass through portals...")))
