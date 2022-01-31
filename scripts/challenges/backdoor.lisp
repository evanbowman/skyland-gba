;;;
;;; backdoor.lisp
;;;


(coins-add 5000)


(terrain (player) 6)
(island-configure
 (player)
 '((power-core 1 13)
   (workshop 3 13)))


(chr-add (player) 2 14 'neutral 0)




(opponent-init 8 'hostile)


(island-configure
 (opponent)
 '((forcefield 1 13)
   (forcefield 1 12)
   (forcefield 1 14)
   (hull 2 9)
   (forcefield 2 13)
   (forcefield 2 14)
   (hull 2 7)
   (forcefield 2 12)
   (hull 2 8)
   (hull 2 10)
   (hull 3 6)
   (hull 3 7)
   (forcefield 3 11)
   (energized-hull 3 10)
   (power-core 3 8)
   ;; NOTE: intentionally damaged
   (nemesis 4 12 40)
   (nemesis 4 14 40)
   (nemesis 4 13 40)
   (hull 4 6)
   (ion-fizzler 4 10)
   (hull 4 7)
   (reactor 5 9)
   (hull 5 8)
   (hull 5 7)
   (energized-hull 6 12)
   (energized-hull 6 13)
   (energized-hull 6 14)
   (hull 6 8)
   (hull 6 7)
   (hull 7 12)
   (hull 7 11)
   (hull 7 10)
   (hull 7 9)
   (hull 7 8)))
