;;;
;;; game_speed.lisp
;;;

;;; NOTE: ordering of results in the (rooms) function is not deterministic when
;;; rewinding game logic. hence the difference function.
(assert-eq
 nil
 (difference
  (rooms (player))
 '((power-core 1 13) (power-core 1 11) (cannon 3 14) (hull 3 11) (cannon 3 12) (cannon 3 13) (flak-gun 3 10) (forcefield 4 13) (hull 4 11) (forcefield 4 14) (forcefield 4 12))))

(assert-eq
 nil
 (difference
  (rooms (opponent))
  '((mirror-hull 0 13) (fire-charge 0 12) (fire-charge 0 11) (mirror-hull 0 10) (mirror-hull 0 14) (mirror-hull 1 13) (mirror-hull 1 14) (mirror-hull 1 10) (power-core 2 13) (power-core 2 11) (stacked-hull 2 10) (stacked-hull 3 10) (missile-silo 4 13) (missile-silo 5 13))))


(assert-eq
 (chrs (player))
 '())

(assert-eq
 (groups)
 '((Up) (Left) (Right) (poweroff)))

(assert-eq 2300 (coins))
