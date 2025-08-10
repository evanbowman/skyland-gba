;;;
;;; challenges/porcupine1.lisp
;;;


(coins-set 10000)


(terrain-set (player) 6)
(island-configure
 (player)
 '((power-core 1 13)
   (workshop 3 13)))

(chr-new (player) 2 14 'neutral nil)


(defn challenge-hint ()
  (dialog "Are you sure you want a hint?")
  (dialog-await-y/n)

  (defn on-dialog-accepted ()
    (dialog "Clear out some of that masonry, then, flak-guns might be useful..."))

  (setq on-dialog-declined (lambda ())))

(flag-show (player) 0)


(opponent-init 7 'hostile)

(island-configure
 (opponent)
 '((hull 0 14)
   (cannon 0 10)
   (cannon 0 11)
   (masonry 0 12)
   (cannon 0 13)
   (hull 0 6)
   (cannon 0 7)
   (cannon 0 8)
   (masonry 0 9)
   (forcefield 1 5)
   (masonry 1 9)
   (hull 1 10)
   (hull 1 14)
   (missile-silo 1 6)
   (masonry 1 12)
   (masonry 1 13)
   (hull 1 8)
   (masonry 1 11)
   (hull 2 13)
   (energized-hull 2 14)
   (masonry 2 9)
   (hull 2 10)
   (masonry 2 12)
   (hull 2 6)
   (hull 2 11)
   (energized-hull 2 7)
   (hull 2 8)
   (forcefield 3 5)
   (missile-silo 3 6)
   (hull 3 14)
   (hull 3 11)
   (energized-hull 3 13)
   (masonry 3 9)
   (masonry 3 12)
   (energized-hull 3 8)
   (energized-hull 3 10)
   (hull 4 6)
   (hull 4 8)
   (energized-hull 4 11)
   (hull 4 10)
   (masonry 4 12)
   (hull 4 13)
   (hull 4 14)
   (masonry 4 9)
   (hull 4 7)
   (power-core 5 7)
   (power-core 5 13)
   (stacked-hull 5 5)
   (power-core 5 10)
   (stacked-hull 5 6)
   (masonry 5 12)
   (masonry 5 9)
   (hull 6 12)
   (hull 6 9)
   (stacked-hull 6 6)
   (stacked-hull 6 5)))

(chr-new (opponent) 5 14 'hostile 0)
(flag-show (opponent) 0)


(defn on-victory ()
  (challenge-complete 5))


(weather-set 3)
