;;;
;;; quest_marker/goblin_pickup.lisp
;;;


(if (chance 4)
    (eval-file "scripts/event/quest_marker/goblin_pickup_1.lisp")
  (eval-file "scripts/event/quest_marker/goblin_pickup_0.lisp"))
