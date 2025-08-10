;;;
;;; challenges/arcgun_defense.lisp
;;;


(defn challenge-hint ()
  (dialog "Sorry, no hints for this one."))


(defn on-victory ()
  (challenge-complete 8))


(terrain-set (player) 6)
(island-configure
 (player)
 '((missile-silo 0 13)
   (power-core 1 13)
   (bronze-hull 1 11)
   (hull 1 12)
   (bronze-hull 2 11)
   (hull 2 12)
   (bronze-hull 3 14)
   (bronze-hull 3 13)
   (bronze-hull 3 12)
   (bronze-hull 3 11)
   (bronze-hull 3 10)
   (bronze-hull 3 9)
   (hull 4 9)
   (hull 4 10)
   (hull 4 11)
   (hull 4 12)
   (hull 4 13)
   (hull 4 14)))

(coins-set 800)


(flag-show (player) 0)



(opponent-init 9 'hostile)


(island-configure
 (opponent)
 '((arc-gun 0 14) (bronze-hull 0 7) (arc-gun 0 6) (bronze-hull 0 13) (bronze-hull 0 9) (arc-gun 0 10) (bronze-hull 0 11) (arc-gun 0 12) (arc-gun 0 8) (hull 1 14) (hull 1 6) (hull 1 10) (bronze-hull 1 7) (bronze-hull 1 11) (hull 1 12) (bronze-hull 1 13) (hull 1 8) (bronze-hull 1 9) (hull 2 13) (hull 2 14) (hull 3 14) (hull 4 13) (hull 4 14) (hull 5 13) (hull 5 14) (hull 5 11) (hull 5 12) (hull 5 10) (power-core 6 13) (power-core 6 11) (hull 6 10) (hull 6 9) (hull 7 10) (hull 7 9) (hull 7 8)))



(weather-set 3)
