(assert-eq (rooms (player))
           '((infirmary 0 13) (portal 2 14) (power-core 3 13) (workshop 3 11) (portal 3 10) (reactor 4 8) (portal 5 12) (manufactory 5 13) (hull 6 12) (missile-silo 6 10) (portal 7 12) (hull 7 11) (reactor 7 8) (stairwell 8 11)))

(assert-eq (chrs (player)) '((7 10 (id . 1)) (4 12 (sc . 10) (id . 2))))
