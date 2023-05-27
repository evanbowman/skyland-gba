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

(flag-show (opponent) 1)


(chr-new (opponent) 1 14 'neutral 0)
(chr-new (opponent) 2 14 'neutral 0)


(setq on-converge
      (lambda
        (dialog "One of the mercenaries offers to join you crew, for a cost of "
                (string (* 600 (zone)))
                "@. Accept offer?")

        (dialog-await-y/n)
        (setq on-converge nil)))


(setq on-dialog-accepted
      (lambda

        (setq temp (chr-slots (player)))

        (if (> (* 400 (zone)) (coins))
            (dialog "You cannot afford to pay. The mercenaries become impatient, and cut the transmission.")
          (if temp
              (progn
                (coins-add (* -400 (zone)))
                (setq temp (get temp (choice (length temp))))
                (chr-new (player) (car temp) (cdr temp) 'neutral 0)
                (chr-del (opponent) 1 14)
                (setq temp (nil))
                (dialog "The mercenary joined your crew!")
                (adventure-log-add 27 (list (* 400 (zone)))))
            (dialog "Sadly, there's no room...")))

        (exit)))


(setq on-dialog-declined
      (lambda
        (dialog "The mercenaries became angry, and cut the transmission.")
        (adventure-log-add 28 '())
        (exit)))
