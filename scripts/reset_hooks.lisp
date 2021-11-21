;;;
;;; reset_hooks.lisp
;;;


(def after-fadein-hook '())
(def after-converge-hook '())
(def after-dialog-accepted-hook '())
(def after-dialog-declined-hook '())
(def hostile-transition-hook '())


(if (not (bound 'last-zone))
    (def last-zone (zone)))


(if (not (bound 'enemies-seen))
    (def enemies-seen '()))


(if (not (bound 'friendlies-seen))
    (def friendlies-seen '()))


(gc)
