;;;
;;; multi_init.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(coins-add -999999)


(terrain (player) 8)


(island-configure
 (player)
 '((power-core 1 13)))


(chr-add (player) 1 14 'neutral 0)
(chr-add (player) 2 14 'neutral 0)





(opponent-init 8 'hostile)


(show-flag (opponent))


(island-configure
 (opponent)
 '((power-core 5 13)))


(chr-add (opponent) 6 14 'hostile 0)
(chr-add (opponent) 5 14 'hostile 0)
