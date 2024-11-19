;;;
;;; decimator.lisp
;;;

(defn challenge-hint ()
  (dialog "Are you sure you want a hint?")
  (dialog-await-y/n)

  (defn on-dialog-accepted ()
    (dialog "Hint: Decimators will only fire in a straight line."))

  (setq on-dialog-declined (lambda ())))


(terrain-set (player) 10)
(island-configure
 (player)
 '((power-core 1 13)
   (manufactory 3 13)))

(flag-show (player) 0)

(coins-add 4300)



(opponent-init 10 'hostile)


(island-configure
 (opponent)
 '((mirror-hull 0 7)
   (mirror-hull 0 8)
   (forcefield* 0 13)
   (forcefield* 0 12)
   (forcefield 0 11)
   (forcefield* 0 14)
   (forcefield 0 10)
   (forcefield* 0 5)
   (hull 0 4)
   (forcefield* 0 6)
   (forcefield* 0 9)
   (decimator 3 9)
   (decimator 3 13)
   (stacked-hull 3 4)
   (decimator 3 5)
   (decimator 3 11)
   (hull 4 8 60)
   (hull 4 7 60)
   (stacked-hull 4 4)
   (power-core 5 13)
   (power-core 5 9)
   (power-core 5 7)
   (power-core 5 11)
   (hull 5 6)
   (hull 5 5)
   (hull 6 5)
   (hull 6 6)
   (power-core 7 9)
   (stairwell 7 11)
   (hull 7 6)
   (reactor 8 6)
   (hull 9 10)
   (hull 9 9)
   (ion-fizzler 7 7)
   (hull 8 14)
   (hull 8 5)
   (stacked-hull 8 4)
   (stacked-hull 9 4)
   (hull 9 5)
   (hull 8 12)
   (hull 8 13)
   (hull 8 11)
   (hull 9 14)
   (hull 9 13)
   (hull 9 12)
   (hull 9 11)
   (forcefield 1 13)))


(map
 (lambda (xy)
   (chr-new (opponent) (first xy) (second xy) 'hostile 0))
 '((4 . 6)
   (4 . 14)
   (5 . 14)
   (6 . 14)
   (7 . 14)))



(weather-set 3)


(defn on-victory ()
  (challenge-complete 7))
