;;;
;;; challenges/beacon.lisp
;;;


(setq on-fadein
      (lambda ()
        (dialog "<c:Crew:5>Those goblins noticed us and are about to jump away to bring help! Destroy them before they can get away!")))


(defn challenge-hint ()
  (dialog "Are you sure you want a hint?")
  (dialog-await-y/n)

  (defn on-dialog-accepted ()
    (dialog "Cannons and missiles aren't going to work here. You'll need to find a way to punch a hole inside the enemy's walls and deploy a drone..."))

  (setq on-dialog-declined (lambda ())))


(defn on-victory ()
  (challenge-complete 9))


(terrain-set (player) 5)
(island-configure
 (player)
 '((power-core 1 13)
   (workshop 3 13)))

(chr-new (player) 2 14 'neutral 0)


(coins-add 3000)


(flag-show (player) 0)



(opponent-init 9 'hostile)


(island-configure
 (opponent)
 '((stacked-hull 0 6)
   (hull 0 7)
   (bronze-hull 0 11)
   (hull 0 9)
   (bronze-hull 0 13)
   (mirror-hull 0 10)
   (mirror-hull 0 14)
   (mirror-hull 0 12)
   (stacked-hull 0 8)
   (stacked-hull 1 7)
   (hull 1 6)
   (bronze-hull 1 10)
   (hull 1 8)
   (stacked-hull 1 9)
   (mirror-hull 1 11)
   (bronze-hull 1 12)
   (bronze-hull 1 14)
   (mirror-hull 1 13)
   (hull 2 7)
   (mirror-hull 2 10)
   (bronze-hull 2 11)
   (mirror-hull 2 12)
   (mirror-hull 2 14)
   (bronze-hull 2 13)
   (hull 2 9)
   (stacked-hull 2 6)
   (stacked-hull 2 8)
   (bronze-hull 3 12)
   (mirror-hull 3 13)
   (bronze-hull 3 10)
   (stacked-hull 3 7)
   (mirror-hull 3 11)
   (stacked-hull 3 9)
   (bronze-hull 3 14)
   (hull 3 8)
   (hull 3 6)
   (hull 4 10)
   (stacked-hull 4 8)
   (escape-beacon 4 12)
   (stacked-hull 4 6)
   (hull 4 11)
   (hull 4 9)
   (hull 4 7)
   (hull 5 13)
   (stacked-hull 5 7)
   (stacked-hull 5 9)
   (power-core 5 11)
   (hull 5 6)
   (hull 5 14)
   (hull 5 8)
   (hull 5 10)
   (stacked-hull 6 10)
   (ladder 6 13)
   (stacked-hull 6 8)
   (stacked-hull 6 6)
   (hull 6 7)
   (hull 6 9)
   (power-core 7 13)
   (hull 7 6)
   (stacked-hull 7 7)
   (stacked-hull 7 9)
   (reactor 7 10)
   (hull 7 8)
   (hull 8 7)
   (hull 8 9)
   (stacked-hull 8 6)
   (stacked-hull 8 8)))


(click (opponent) 4 12)

(weather-set weather-id-rain)


(foreach
 (lambda (xy)
   (chr-new (opponent) (first xy) (second xy) 'hostile 0))
 '((7 . 12)
   (8 . 12)))
