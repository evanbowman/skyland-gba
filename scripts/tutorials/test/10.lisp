(assert-eq
 (rooms (player))
 '((stairwell 0 11) (hull 0 8) (workshop 1 13) (hull 3 14) (hull 3 13) (power-core 3 11) (hull 4 10) (flak-gun 4 13) (flak-gun 4 14) (hull 5 10) (hull 5 11) (hull 5 12) (forcefield 6 14)))

(assert-eq
 (chrs (player))
 '((4 12 (id . 1)) (3 12 (id . 2))))

(assert-eq
 (coins)
 1230)
