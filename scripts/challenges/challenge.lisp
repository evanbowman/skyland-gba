;;;
;;; challenge.lisp
;;;

(eval-file "/scripts/reset_hooks.lisp")

(if (bound 'challenge-hint) (unbind 'challenge-hint))

(gc)

'(("1: Goliath" . "challenges/goliath.lisp")
  ("2: Goblin Raid" . "challenges/goblin_raid.lisp")
  ("3: Nemesis" . "challenges/backdoor.lisp")
  ("4: Masonry" . "challenges/masonry.lisp")
  ("5: Mycelium!" . "challenges/mycelium.lisp")
  ("6: Porcupine" . "challenges/porcupine.lisp"))
