;;;
;;; challenges/backdoor.lisp
;;;


(coins-add 5300)

(defn on-victory ()
  (challenge-complete 2))


(defn challenge-hint ()
  (dialog "Are you sure you want a hint?")
  (dialog-await-y/n)

  (defn on-dialog-accepted ()
    (dialog "Hint: You may need to use a repair-drone in an unconventional way. Also: Read the description for nemesis in the game's glossary.")

    (push-menu "glossary" '(nemesis)))

  (setq on-dialog-declined (lambda ())))


(terrain-set (player) 6)
(island-configure
 (player)
 '((power-core 1 13)
   (workshop 3 13)))


(chr-new (player) 2 14 'neutral nil)




(opponent-init 8 'hostile)


(island-configure
 (opponent)
 '((hull 1 10)
   (hull 1 9)
   (hull 1 8)
   (forcefield 1 13)
   (forcefield 1 12)
   (forcefield 1 14)
   (energized-hull 2 9)
   (forcefield 2 13)
   (forcefield 2 14)
   (hull 2 7)
   (forcefield 2 12)
   (hull 2 8)
   (energized-hull 2 10)
   (hull 3 6)
   (hull 3 7)
   (hull 3 11)
   (forcefield 2 11)
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
   (hull 6 12)
   (energized-hull 6 13)
   (energized-hull 6 14)
   (hull 6 8)
   (hull 6 7)
   (hull 7 12)
   (hull 7 11)
   (hull 7 10)
   (hull 7 9)
   (hull 7 8)))

(chr-new (opponent) 5 11 'hostile 0)


(weather-set weather-id-rain)
