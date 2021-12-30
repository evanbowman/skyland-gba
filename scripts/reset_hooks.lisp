;;;
;;; reset_hooks.lisp
;;;


(setq after-fadein-hook '())
(setq after-converge-hook '())
(setq after-dialog-accepted-hook '())
(setq after-dialog-declined-hook '())
(setq hostile-transition-hook '())


(if (not (bound 'last-zone))
    (setq last-zone (zone)))


(if (not (bound 'enemies-seen))
    (setq enemies-seen '()))


(if (not (bound 'friendlies-seen))
    (setq friendlies-seen '()))


(gc)
