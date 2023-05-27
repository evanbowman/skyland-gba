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
        (let ((temp (chr-slots (player)))
              (join (lambda
                      (adventure-log-add 7 '())
                      (dialog $0))))
          (if temp
              (progn
                (setq temp (get temp (choice (length temp))))
                (chr-new (player) (car temp) (cdr temp) 'neutral 0)
                (chr-del (opponent) 1 14)
                (if (or (equal (choice 2) 1) (< (coins) 300))
                    (join "The castaway joined your crew!")
                  (progn
                    (coins-set (- (coins) 300))
                    (join "The castaway joined your crew. "
                          "Starving, he ate 300@ of your food supplies!"))))
            (progn
              (dialog "Sadly, there's no room...")
              (defn on-dialog-closed
                (dialog "<c:castaway:1>Hold on, don't leave me here! I may not meet anyone else for a long time... I'll help you build an addition onto your castle, then there'll be enough space for me to sleep! Let's see... I've got just enough supplies to build a ladder...")
                (defn on-dialog-closed
                  (while (< (length (construction-sites (player) '(1 . 2))) 1)
                    (terrain (player) (+ (terrain (player)) 1)))
                  (sel-input 'ladder
                             "Place ladder (1x2):"
                             (lambda
                               (syscall "sound" "build0")
                               (room-new (player) `(ladder ,$1 ,$2))
                               (chr-del (opponent) 1 14)
                               (chr-new (player) $1 (+ 1 $2) 'neutral 0)
                               (dialog "<c:castaway:1> Thanks for rescuing me! I'll try to help out however I can!")
                               (defn on-dialog-closed
                                 (join "The castaway joined your crew!")
                                 (setq on-dialog-closed nil)
                                 (exit)))))))))
        (exit)))


(setq on-dialog-declined
      (lambda
        (exit)))
