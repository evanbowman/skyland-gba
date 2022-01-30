;;;
;;; neutral/1/0.lisp
;;;


(dialog "You discover a fortress inhabited by some mercenaries...")


(opponent-init 8 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (missile-silo 5 13)
   (missile-silo 6 13)
   (missile-silo 7 13)))


(chr-add (opponent) 1 14 'neutral 0)
(chr-add (opponent) 2 14 'neutral 0)


(setq on-converge
      (lambda
        (dialog "One of the mercenaries offers to join you crew, for a cost of "
                (string (* 400 (zone)))
                "@. Accept offer?")

        (dialog-await-y/n)
        (setq on-converge nil)))


(setq on-dialog-accepted
      (lambda

        (setq temp (chr-slots (player)))

        (if (> (* 400 (zone)) (coins))
            (dialog "You cannot afford to pay. The mercenaries become frustrated, and cut the transmission.")
          (if temp
              (progn
                (coins-add (* -400 (zone)))
                (setq temp (get temp (choice (length temp))))
                (chr-add (player) (car temp) (cdr temp) 'neutral 0)
                (chr-rem (opponent) 1 14)
                (setq temp (nil))
                (dialog "The mercenary joined your crew!"))
            (dialog "Sadly, there's no room...")))

        (exit-level)))


(setq on-dialog-declined
      (lambda
        (dialog "The mercenaries became angry, and cut the transmission.")
        (exit-level)))
