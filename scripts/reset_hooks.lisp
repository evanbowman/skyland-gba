;;;
;;; reset_hooks.lisp
;;;


(setq on-fadein '())
(setq on-converge '())
(setq on-dialog-accepted '())
(setq on-dialog-declined '())
(setq on-dialog-closed '())
(setq on-hostile-transition '())
(setq on-victory '())
(setq on-room-destroyed '())



(if (not (bound 'last-zone))
    (setq last-zone (zone)))


(if (not (bound 'enemies-seen))
    (setq enemies-seen '()))


(if (not (bound 'friendlies-seen))
    (setq friendlies-seen '()))


(gc)
