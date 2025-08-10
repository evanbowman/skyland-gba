;;;
;;; challenges/fire_brigade.lisp
;;;


(coins-add 4100)


(defn on-victory ()
  (challenge-complete 12))


(defn challenge-hint ()
  (dialog "Sorry, no hints for this one."))


(terrain-set (player) 7)
(island-configure
 (player)
 '((mycelium 0 7) (hull 0 8) (replicator 0 9) (power-core 0 13) (mycelium 1 8) (mycelium 1 7) (mycelium 2 7) (mycelium 2 10) (hull 2 8) (stairwell 2 11) (mycelium 2 9) (mycelium 3 10) (power-core 3 11) (mycelium 3 8) (mycelium 3 9) (mycelium 3 13) (mycelium 3 14) (mycelium 3 7) (mycelium 4 13) (mycelium 4 10) (mycelium 4 14) (mycelium 5 10) (mycelium 5 13) (mycelium 5 12) (hull 5 14) (mycelium 5 11) (cannon 6 14) (cannon 6 12) (cannon 6 13)))

(flag-show (player) 0)

(chr-new (player) 1 14 'neutral nil)


(opponent-init 9 'hostile)


(island-configure
 (opponent)
 '((forcefield 0 11) (forcefield* 0 10) (forcefield 0 9) (hull 0 14) (forcefield 0 13) (forcefield* 0 12) (energized-hull 1 13) (forcefield* 1 11) (forcefield* 1 9) (forcefield 1 12) (energized-hull 1 14) (forcefield 1 10) (forcefield* 2 8) (forcefield 2 9) (forcefield* 2 12) (hull 2 14) (forcefield 2 11) (forcefield* 2 10) (hull 2 13) (incinerator 3 11) (incinerator 3 9) (stacked-hull 3 7) (stacked-hull 3 8) (power-core 3 13) (stacked-hull 4 8) (stacked-hull 4 7) (forcefield 5 5) (forcefield* 5 6) (stacked-hull 5 10) (war-engine 5 11) (rocket-bomb 5 7) (stacked-hull 6 10) (forcefield* 6 6) (forcefield 6 5) (rocket-bomb 6 7) (stacked-hull 7 10) (forcefield* 7 6) (forcefield 7 5) (rocket-bomb 7 7)))


(chr-new (opponent) 6 14 'hostile 0)
(chr-new (opponent) 5 14 'hostile 0)


(weather-set 4)
