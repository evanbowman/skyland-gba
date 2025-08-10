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
 '((584 Select)
   (133 Select)
   (684 Right)
   (968 A)
   (333 Right)
   (233 Right)
   (634 A)
   (1234 Right)
   (1000 nil)
   (100 "<c:Milo:5>This is a replicator, it will allow you to duplicate an existing crew member for 700@!")
   (1135 A)
   (1033 A)
   (3590 A)
   (233 Right)
   (651 A)
   (801 Left)
   (801 A)
   (233 A)
   (701 A)
   (216 Left)
   (200 Left)
   (467 A)
   (1500 nil)
   (100 "<c:Milo:5>Pretty cool! right? <B:0> There is no limit to the number of replicants you can create, but beware: replicants have less HP, and they cannot be healed.")
   (3055 Right)
   (801 A)
   (317 Left)
   (217 Left)
   (350 A)
   (300 Right)
   (217 Right)
   (216 Right)
   (267 Right)
   (350 A)
   (183 Left)
   (300 A)
   (267 Left)
   (1736 A)
   (250 A)
   (1800 nil)
   (100 "<c:Milo:5>You can even create replicants from replicants, but they will be even weaker than the replicant that they were created from!")))
