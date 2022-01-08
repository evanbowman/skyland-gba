;;;
;;; sandbox.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(if (not (bound 'sandbox-msg))
    (progn
      (task
       1000
       (lambda
         (dialog "Welcome to the SKYLAND Battle Sandbox! Sandbox mode gives you nearly unlimited coins, and allows you to build on your opponent's island in addition to your own! You may also reposition your opponent's characters!")))
      (setq sandbox-msg 1)))



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
