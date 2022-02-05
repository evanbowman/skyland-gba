;;;
;;; neutral/1/1.lisp
;;;


(dialog "A castle lazily drifts across the horizon... possibly uninhabited?")



(opponent-init 5 'hostile)


(island-configure
 (opponent)
 '((hull 0 14)
   (power-core 1 13)
   (hull 1 12)
   (workshop 3 13)))


(setq on-converge
      (lambda
        (dialog "The fortress appears to be empty, but you cannot be certain. Attempt to board?")
        (setq on-converge '())
        (dialog-await-y/n)))


(setq on-dialog-accepted
      (lambda
        (if (equal (choice 2) 0)
            (progn
              (setq temp (+ 600 (choice 300)))
              (dialog "You explore, and salvage " (string temp) "@ from the ruins.")
              (coins-add temp)
              (exit))
          (progn
            (island-configure
             (opponent)
             '((hull 2 12)
               (cannon 0 13)
               (arc-gun 0 14)
               (power-core 1 13)
               (hull 1 12)
               (missile-silo 3 13)
               (missile-silo 4 13)))
            (show-flag (opponent))
            (dialog "It's a trap!")))))


(setq on-dialog-declined
      (lambda
        (dialog "The fortress sinks back into the clouds, its contents remain an unresolved mystery.")
        (exit)))
