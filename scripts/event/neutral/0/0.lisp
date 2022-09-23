;;;
;;; neutral/0/0.lisp
;;;


(dialog "In the distance, you see an island inhabited by a lone castaway...")


(opponent-init 6 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (coconut-palm 5 12)
   (hull 5 14)))


(secret
 5 14
 "Days stranded: |||| |||| |||| |||| ||||...")


(chr-new (opponent) 1 14 'neutral 0)


(setq on-converge
      (lambda
        (dialog
         "<c:castaway:1>Fancy meeting you here! I've been marooned on this island... "
         "who knows how long! Looks like a nasty storm's brewing, mind "
         "if I hitch a ride?")

        (setq on-dialog-closed
              (lambda
                (dialog "He seems harmless, invite him aboard?")
                (dialog-await-y/n)
                (setq on-dialog-closed '())))

        (setq on-converge nil)))


(setq on-dialog-accepted
      (lambda

        (setq temp (chr-slots (player)))

        (if temp
            (progn
              (setq temp (get temp (choice (length temp))))
              (chr-new (player) (car temp) (cdr temp) 'neutral 0)
              (chr-del (opponent) 1 14)
              (setq temp '())
              (dialog "The castaway joined your crew!"))
          (progn
            (dialog "Sadly, there's no room...")))

        (exit)))


(setq on-dialog-declined
      (lambda
        (exit)))
