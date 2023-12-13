

(dialog "A damaged fortress floats into view. The residents do not respond to your radio signals.")


(opponent-init 5 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 2 13)
   (hull 2 14)
   (hull 3 12)
   (hull 4 12)))


(chr-new (opponent) 1 14 'neutral 0)


(setq on-converge
      (lambda
        (dialog "You see a survivor amongst the wreckage. You cannot be sure whether the"
                " survivor is trustworthy. Invite survivor aboard?")

        (dialog-await-y/n)
        (setq on-converge nil)))


(let ((bad (choice 2)))

  (if (< (zone) 2)
      (secret
       4 12
       (if bad
           "Humans eaten: 17"
         "Days alone on island: lll")))


  (setq on-dialog-accepted
        (lambda

          (let ((temp (chr-slots (player))))
            (setq temp (get temp (choice (length temp))))

            (if temp
                (progn
                  (chr-del (opponent) 1 14)
                  (if (not bad)
                      (progn
                        (chr-new (player) (car temp) (cdr temp) 'neutral nil)
                        (dialog "The survivor joined your crew!")
                        (adventure-log-add 40 '())
                        (exit))
                    (progn
                      (chr-new (player) (car temp) (cdr temp) 'hostile nil)
                      (dialog "The survivor turned out to be a vicious goblin!")
                      (adventure-log-add 41 '())
                      (setq on-dialog-closed
                            (lambda
                              (dialog "<c:goblin:2>Die humansss!")
                              (setq on-dialog-closed '()))))))
              (progn
                (dialog "Sadly, there's no room...")
                (exit)))))))



(setq on-dialog-declined
      (lambda
        ;; TODO...
        (exit)))
