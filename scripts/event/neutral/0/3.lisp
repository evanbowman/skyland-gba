;;;
;;; neutral/0/4.lisp
;;;


(dialog "A distress signal sounds over your radio. You change course to investigate...")


(opponent-init 5 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 0 14)))


(chr-new (opponent) 1 14 'neutral 0)
(chr-new (opponent) 2 14 'neutral 0)


(setq on-converge
      (lambda
        (dialog "You discover a damaged fortress. Spend 800@ in resources to rescue the survivors?")
        (dialog-await-y/n)
        (setq on-converge nil)))


(setq on-dialog-accepted
      (lambda
        (setq temp (chr-slots (player)))

        (if (> 800 (coins))
            (dialog "Unfortunately, you do not have enough resources.")
          (if temp
              (progn
                (coins-add -800)
                (setq temp (get temp (choice (length temp))))
                (chr-new (player) (car temp) (cdr temp) 'neutral 0)
                (chr-del (opponent) 1 14)

                (setq temp (chr-slots (player)))
                (if temp
                    (progn
                      (setq temp (get temp (choice (length temp))))
                      (chr-new (player) (car temp) (cdr temp) 'neutral 0)
                      (chr-del (opponent) 2 14)
                      (dialog "Two survivors joined your crew!"))
                  (dialog "You rescue one of the survivors. The other survivor decides "
                          "that your castle is too crowded and declines to come aboard.")))
            (dialog "Sadly, there's no room in your castle for anyone else")))

        (exit)))


(setq on-dialog-declined
      (lambda
        ;; TODO...
        (exit)))
