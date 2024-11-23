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
   (100 "<c:Milo:5>This is a replicator, it will allow you to duplicate an existing crew member for 700@!")
   (100 "<c:Milo:5>pretty cool! right?")   
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
   (100 "<c:Milo:5>There is no limit to the number of replicants you can create, but beware: replicants have less HP, and they cannot be healed.")
   (267 Right)
   (183 Right)
   (216 Right)))
