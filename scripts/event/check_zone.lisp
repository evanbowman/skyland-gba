
(when (not (equal (zone) last-zone))
  (setq friendlies-seen '())
  (setq enemies-seen '())
  (setq shop-items '())
  (when (> (zone) 0)
    (adventure-log-add 26 (list (+ (zone) 1)))))

(setq last-zone (zone))
