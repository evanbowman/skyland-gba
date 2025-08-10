;;;
;;; power_balance.lisp
;;;


(assert-eq
 (rooms (player))
 '((power-core 1 13) (hull 1 12) (hull 2 12) (cannon 5 14) (cannon 5 13) (cannon 5 12) (cannon 5 11 100)))

(assert-eq
 (rooms (opponent))
 '((hull 0 14) (hull 0 13 80) (hull 0 12 40) (hull 0 11 120) (hull 1 14) (hull 1 13) (hull 1 11) (power-core 2 13) (missile-silo 4 13)))
