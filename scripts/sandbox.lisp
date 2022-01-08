;;;
;;; sandbox.lisp
;;;


(eval-other-file "/scripts/reset_hooks.lisp")


(coins-add 10000000)


(terrain (player) 4)


(island-configure
 (player)
 '((power-core 1 13)))


(chr-add (player) 1 14 'neutral 0)




(opponent-init 4 'hostile)


(show-flag (opponent))


(island-configure
 (opponent)
 '((power-core 1 13)))


(chr-add (opponent) 1 14 'hostile 0)
(chr-add (opponent) 2 14 'hostile 0)
