;;;
;;; skyland_forever.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(terrain-set (player) 4)

(island-configure
 (player)
 '((power-core 1 13)
   (ladder 0 13)
   (hull 0 12)))


(map (lambda (x)
       (chr-new (player) x 14 'neutral 0))
     (range 0 3))

(flag-show (player) 0)

(setvar "powerdown_allowed" 0)
(setvar "rewind_disabled" 0)
