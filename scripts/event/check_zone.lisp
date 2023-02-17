
(if (not (equal (zone) last-zone))
    (progn
      (setq friendlies-seen '())
      (setq enemies-seen '())
      (setq shop-items '())))

(setq last-zone (zone))
