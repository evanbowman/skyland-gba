;;;
;;; neutral/0/0.lisp
;;;


(dialog "In the distance, you see an island inhabited by a lone castaway...")


(opponent-init 6 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (coconut-palm 5 13)))


(chr-add (opponent) 1 14 'neutral 0)


(setq after-converge-hook
      (lambda
        (dialog
         "Fancy meeting you here! I've been marooned on this island... "
         "who knows how long! Looks like a nasty storm's brewing, mind "
         "if I hitch a ride?")

        (dialog-decor "castaway" 1)

        (task
         1000
         (lambda
           (dialog "He seems harmless, invite him aboard?")
           (dialog-await-y/n)))

        (setq after-converge-hook nil)))


(setq after-dialog-accepted-hook
      (lambda

        (setq temp (chr-slots (player)))

        (if temp
            (progn
              (setq temp (get temp (choice (length temp))))
              (chr-add (player) (car temp) (cdr temp) 'neutral 0)
              (chr-rem (opponent) 1 14)
              (setq temp '())
              (dialog "The castaway joined your crew!"))
          (progn
            (dialog "Sadly, there's no room...")))

        (exit-level)))


(setq after-dialog-declined-hook
      (lambda
        (exit-level)))
