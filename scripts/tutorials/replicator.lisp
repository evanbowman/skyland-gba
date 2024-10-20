;;;
;;; replicator.lisp
;;;

(terrain-set (player) 6)
(island-configure
 (player)
 '((power-core 1 13)
   (replicator 3 12)
   (stairwell 5 11)))


(chr-new (player) 1 14 'neutral 0)



(coins-add 20000)


(opponent-init 4 'hostile)

(island-configure
 (opponent)
 '((power-core 1 13)
   (stairwell 3 11)))

(chr-new (opponent) 1 14 'hostile 0)


(autopilot
 '((700 Select)
   (20 A)
   (1333 Right)
   (333 A)
   (166 Right)
   (249 Right)
   (383 A)
   (1349 Right)
   (1267 A)
   (1166 A)
   (633 A)
   (166 Right)
   (183 Up)
   (166 Up)
   (233 Up)
   (183 A)
   (233 Down)
   (166 Down)
   (133 Down)
   (282 Left)
   (300 A)
   (632 A)
   (517 A)
   (149 Right)
   (232 Up)
   (217 Up)
   (149 A)
   (200 Down)
   (233 Left)
   (383 Down)
   (199 A)
   (582 A)
   (517 A)
   (116 Right)
   (366 Up)
   (317 A)
   (466 Left)
   (200 Down)
   (282 A)
   (483 A)
   (583 A)
   (166 Right)
   (317 A)
   (299 Left)
   (783 A)
   (1366 A)
   (1783 A)
   (250 Left)
   (166 Left)
   (350 Left)
   (182 A)
   (267 Right)
   (183 Right)
   (216 Right)
   (500 A)
   (683 A)
   (485 A)
   (148 Left)
   (183 Left)
   (283 A)
   (366 Right)
   (183 Right)
   (749 A)
   (483 A)
   (2034 Left)
   (1000 Left)
   (2666 Right)
   (217 Right)
   (166 Right)
   (199 R)
   (300 Right)
   (533 A)
   (200 A)
   (1816 A)
   (716 Left)
   (150 Left)
   (300 A)
   (566 Down)
   (149 Down)
   (233 B)
   (250 Down)
   (232 Down)
   (200 Down)
   (132 Down)
   (133 Left)
   (183 Left)
   (300 Left)
   (383 Right)
   (350 A)
   (182 Right)
   (150 Right)
   (366 A)
   (183 Left)
   (150 Left)
   (182 Left)
   (1966 Left)
   (483 A)
   (250 Right)
   (167 Right)
   (382 A)
   (233 Left)
   (866 A)
   (150 Left)
   (217 A)
   (150 Right)
   (632 Right)
   (1033 Left)
   (999 A)
   (466 A)
   (2618 Right)
   (1166 Left)
   (250 Left)))
