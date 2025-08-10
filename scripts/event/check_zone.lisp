;;;
;;; check_zone.lisp
;;;


(when (not (equal (zone) last-zone))
  (setq friendlies-seen nil)
  (setq enemies-seen nil)
  (setq zone-shop-items nil)
  (when (> (zone) 0)
    (adventure-log-add 26 (list (+ (zone) 1)))))

(setq last-zone (zone))
