;;;
;;; reset_hooks.lisp
;;;


(set 'after-fadein-hook '())
(set 'after-converge-hook '())
(set 'after-dialog-accepted-hook '())
(set 'after-dialog-declined-hook '())
(set 'hostile-transition-hook '())


(if (not (bound 'last-zone))
    (set 'last-zone (zone)))


(if (not (bound 'enemies-seen))
    (set 'enemies-seen '()))


(if (not (bound 'friendlies-seen))
    (set 'friendlies-seen '()))


(if (not (bound 'quest-log))
    (set 'quest-log '()))
