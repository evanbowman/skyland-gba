;;;
;;; continuous.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(island-configure
 (player)
 '((power-core 1 13)))


(chr-new (player) 1 14 'neutral 0)
(show-flag (player))
