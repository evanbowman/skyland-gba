;;;
;;; neutral/2/1.lisp
;;;


(dialog "A damaged fortress floats into view. The residents do not respond to your radio signals.")


(opponent-init 5 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 2 13)
   (hull 2 14)
   (hull 3 12)
   (hull 4 12)))


(chr-add (opponent) 1 14 'neutral 0)


(setq on-converge
      (lambda
        (dialog "You see a survivor amongst the wreckage. You cannot be sure whether the"
                " survivor is trustworthy. Invite survivor aboard?")

        (dialog-await-y/n)
        (setq on-converge nil)))


(setq on-dialog-accepted
      (lambda

        (setq temp (chr-slots (player)))
        (setq temp (get temp (choice (length temp))))


        (if temp
            (progn
              (chr-rem (opponent) 1 14)
              (if (equal (choice 2) 0)
                  (progn
                    (chr-add (player) (car temp) (cdr temp) 'neutral 0)
                    (dialog "The survivor joined your crew!")
                    (exit-level))
                (progn
                  (chr-add (player) (car temp) (cdr temp) 'hostile 0)
                  (dialog "The survivor turned out to be a vicious goblin!")
                  (setq on-dialog-closed
                        (lambda
                          (dialog "Die humansss!")
                          (dialog-decor "goblin" 2)
                          (setq on-dialog-closed '()))))))
          (progn
            (dialog "Sadly, there's no room...")
            (exit-level)))))


(setq on-dialog-declined
      (lambda
        ;; TODO...
        (exit-level)))
