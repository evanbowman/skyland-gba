;;;
;;; challenge.lisp
;;;


(eval-other-file "reset_hooks.lisp")



;; Initial setup for player's island
(configure-player
 (player)
 '((power-core 1 13)))


(add-chr (player) 2 14)

(set 'challenges-completed '())


(gc)


'(("1: Goliath" . "challenges/goliath.lisp")
  ("2: The Flatiron" . "challenge-2.lisp")
  ("3: Goblin Raid" . "challenge-3.lisp")
  ("4: Wind Fortress" . "...")
  ("5: " . "...")
  ("6: " . "...")
  ("7: " . "...")
  ("8: " . "...")
  ("9: " . "...")
  ("10: " . "...")
  ("11: " . "..."))
