;;;
;;; skyland_forever.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(terrain (player) 4)

(island-configure
 (player)
 '((power-core 1 13)))


(chr-new (player) 1 14 'neutral 0)
(chr-new (player) 2 14 'neutral 0)
(flag-show (player))
