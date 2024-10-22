(assert-eq
 (groups)
 '((Up (3 . 13) (3 . 14)) (Left (0 . 13)) (Right) (poweroff)))

(assert-eq
 (rooms (player))
 '((missile-silo 0 13) (power-core 1 13) (cannon 3 13) (cannon 3 14)))
