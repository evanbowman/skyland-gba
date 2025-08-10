;;;
;;; challenges/porcupine2.lisp
;;;


(coins-set 11000)


(terrain-set (player) 7)
(island-configure
 (player)
 '((power-core 1 13)
   (manufactory 3 13)))

(chr-new (player) 2 14 'neutral nil)


(defn challenge-hint ()
  (dialog "Are you sure you want a hint?")
  (dialog-await-y/n)

  (defn on-dialog-accepted ()
    (dialog "Build a rocket-bomb, defend it well..."))

  (setq on-dialog-declined (lambda ())))


(flag-show (player) 0)


(opponent-init 12 'hostile)

(island-configure
 (opponent)
 '((cannon 0 13)
   (hull 0 10)
   (cannon 0 9)
   (energized-hull 0 14)
   (hull 0 8)
   (cannon 0 11)
   (energized-hull 0 12)
   (energized-hull 1 9)
   (energized-hull 1 13)
   (hull 1 14)
   (hull 1 8)
   (hull 1 10)
   (energized-hull 1 11)
   (hull 1 12)
   (energized-hull 2 10)
   (hull 2 9)
   (energized-hull 2 8)
   (hull 2 11)
   (hull 2 13)
   (energized-hull 2 14)
   (energized-hull 2 12)
   (hull 3 10)
   (hull 3 12)
   (energized-hull 3 13)
   (energized-hull 3 11)
   (energized-hull 3 14)
   (power-core 4 13)
   (hull 4 10)
   (hull 4 9)
   (radiator 4 12)
   (stacked-hull 4 11)
   (stacked-hull 5 11)
   (forcefield* 5 8)
   (missile-silo 5 9)
   (stacked-hull 5 12)
   (stacked-hull 6 14)
   (radiator 7 12)
   (stacked-hull 7 11)
   (hull 7 9)
   (hull 7 10)
   (power-core 7 13)
   (stacked-hull 8 11)
   (forcefield* 8 8)
   (missile-silo 8 9)
   (stacked-hull 8 12)
   (stacked-hull 9 14)
   (reactor 10 12)
   (stacked-hull 10 11)
   (hull 10 10)
   (hull 10 9)
   (forcefield* 11 8)
   (stacked-hull 11 11)
   (missile-silo 11 9)))


(defn on-victory ()
  (challenge-complete 6))


(weather-set 3)
