;;;
;;; neutral/0/4.lisp
;;;


(dialog "A distress signal sounds over your radio. You change course to investigate...")


(opponent-init 5 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 0 14)))


(chr-add (opponent) 1 14 'neutral 0)
(chr-add (opponent) 2 14 'neutral 0)


(setq after-converge-hook
      (lambda
        (dialog "You discover a damaged fortress. Spend 800@ in resources to rescue the survivors?")
        (dialog-await-y/n)
        (setq after-converge-hook nil)))


(setq after-dialog-accepted-hook
      (lambda
        (setq temp (chr-slots (player)))

        (if (> 800 (coins))
            (dialog "Unfortunately, you do not have enough resources.")
          (if temp
              (progn
                (coins-add -800)
                (setq temp (get temp (choice (length temp))))
                (chr-add (player) (car temp) (cdr temp) 'neutral 0)
                (chr-rem (opponent) 1 14)

                (setq temp (chr-slots (player)))
                (if temp
                    (progn
                      (setq temp (get temp (choice (length temp))))
                      (chr-add (player) (car temp) (cdr temp) 'neutral 0)
                      (chr-rem (opponent) 2 14)
                      (dialog "Two survivors joined your crew!"))
                  (dialog "You rescue one of the survivors. The other survivor decides "
                          "that your castle is too crowded and declines to come aboard.")))
            (dialog "Sadly, there's no room in your castle for anyone else")))

        (exit-level)))


(setq after-dialog-declined-hook
      (lambda
        ;; TODO...
        (exit-level)))
