;;;
;;; neutral_1_1.lisp
;;;


(dialog "A castle lazily drifts across the horizon... possibly uninhabited?")



(init-opponent 5 'hostile)


(configure-player
 (opponent)
 '((hull 0 14)
   (power-core 1 13)
   (hull 1 12)
   (workshop 3 13)))


(set 'after-converge-hook
     (lambda
       (dialog "The fortress appears to be empty, but you cannot be certain. Attempt to board?")
       (set 'after-converge-hook '())
       (await-dialog-y/n)))


(set 'after-dialog-accepted-hook
     (lambda
       (if (equal (cr-choice 2) 0)
           (progn
             (set 'temp (+ 600 (cr-choice 300)))
             (dialog "You explore, and salvage " (string temp) "$ from the ruins.")
             (add-coins temp)
             (exit-level))
         (progn
           (configure-player
            (opponent)
            '((hull 0 14)
              (cannon 0 13)
              (power-core 1 13)
              (hull 1 12)
              (missile-silo 3 13)
              (missile-silo 4 13)))
           (show-flag (opponent))
           (dialog "It's a trap!")))))


(set 'after-dialog-declined-hook
     (lambda
       (dialog "The fortress sinks back into the clouds, its contents remain an unresolved mystery.")
       (exit-level)))
