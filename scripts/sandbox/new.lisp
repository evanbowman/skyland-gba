;;;
;;; sandbox.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(if (not (bound 'sandbox-msg))
    (setq on-fadein
          (lambda
            (setq on-fadein nil)
            (setq sandbox-msg 1)
            (dialog "Welcome to the Battle Sandbox! Want any help?")
            (dialog-await-y/n)
            (setq on-dialog-accepted
                  (lambda
                    (dialog "Sandbox mode gives you nearly unlimited coins, and allows you to build on your opponent's island in addition to your own! You may also reposition your opponent's characters! Try out strategies, or just play around!")))
            (setq on-dialog-declined nil))))


(island-configure
 (player)
 '((power-core 1 13)))


(chr-new (player) 1 14 'neutral 0)
(show-flag (player))



(opponent-init (terrain (player)) 'hostile)


(show-flag (opponent))


;; Note: prior to running this script, the game
;; sets the size of the player island's
;; terrain. Retrieve the value, and make the
;; opponent island the same size.
(let ((temp (terrain (player))))
  (island-configure
   (opponent)
   `((power-core ,(- temp 3) 13)))

  (chr-new (opponent) (- temp 3) 14 'hostile 0)
  (chr-new (opponent) (- temp 2) 14 'hostile 0))
