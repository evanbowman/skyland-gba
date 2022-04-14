;;;
;;; sandbox.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(defn sb-help
  (dialog "Sandbox mode gives you nearly unlimited coins, and allows you to build on your opponent's island in addition to your own! You may also reposition your opponent's characters! Try out strategies, or just play around!"))


(if (not (syscall "save-bit-load" 3))
    (setq on-fadein
          (lambda
            (setq on-fadein nil)
            (syscall "save-bit-store" 3 1)
            (dialog "Welcome to the Battle Sandbox! Want any help?")
            (dialog-await-y/n)
            (setq on-dialog-accepted sb-help)
            (setq on-dialog-declined nil))))


(island-configure
 (player)
 '((power-core 1 13)))


(chr-new (player) 1 14 'neutral 0)
(flag-show (player))



(opponent-init (terrain (player)) 'hostile)


(flag-show (opponent))


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
