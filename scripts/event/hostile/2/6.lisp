;;;
;;; hostile/2/6.lisp
;;;


(opponent-init 10 'hostile)



(island-configure
 (opponent)
 '((arc-gun 0 10)
   (mirror-hull 0 9)
   (mirror-hull 0 11)
   (mirror-hull 0 12)
   (mirror-hull 0 14)
   (mirror-hull 0 13)
   (mirror-hull 1 11)
   (hull 1 10)
   (shrubbery 1 9)
   (mirror-hull 1 12)
   (mirror-hull 1 14)
   (mirror-hull 1 13)
   (hull 2 14)
   (energized-hull 2 12)
   (hull 2 10)
   (hull 2 13)
   (energized-hull 2 11)
   (hull 3 9)
   (energized-hull 3 10)
   (power-core 3 11)
   (power-core 3 13)
   (hull 4 10)
   (shrubbery 4 8)
   (hull 4 9)
   (forcefield 5 11)
   (forcefield 5 10)
   (missile-silo 5 12)
   (masonry 5 14)
   (missile-silo 6 12)
   (masonry 6 14)
   (forcefield 6 11)
   (forcefield 6 10)
   (masonry 7 14)
   (missile-silo 7 12)
   (forcefield 7 11)
   (forcefield 7 10)
   (power-core 8 13)
   (hull 8 12)
   (hull 9 12)))


(flag-show (opponent) flag-id-pirate)


(chr-new (opponent) 4 14 'hostile 0)
(chr-new (opponent) 3 14 'hostile 0)
