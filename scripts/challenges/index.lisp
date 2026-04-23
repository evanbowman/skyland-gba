;;;
;;; challenges/index.lisp
;;;

(tr-bind-current)

(map cons
     (list (tr "1: Goliath")
           (tr "2: Goblin Raid")
           (tr "3: Nemesis")
           (tr "4: Masonry")
           (tr "5: Mycelium!")
           (tr "6: Porcupine 1")
           (tr "7: Porcupine 2")
           (tr "8: Highwater")
           (tr "9: Arc Defense")
           (tr "10: Pursuit")
           (tr "11: Exchange")
           (tr "12: Demolition")
           (tr "13: Fire Brigade"))
     ;; NOTE: the game needs to know at startup how many challenge levels there
     ;; are. Loading the index file and localizing the level titles is excessive
     ;; for the purpose of counting challenge levels, so the level paths have
     ;; been move to a separate file.
     (eval-file "/scripts/challenges/levels.lisp"))
