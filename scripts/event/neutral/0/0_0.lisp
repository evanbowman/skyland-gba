;;;
;;; neutral/0/0_0.lisp
;;;


(dialog
 "In the distance, you see an island inhabited by a lone castaway...")


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


(defn on-converge ()
  (dialog
   "<c:Castaway:1>Fancy meeting you here! I've been marooned on this island... "
   "who knows how long! Looks like a nasty storm's brewing, mind "
   "if I hitch a ride?")

  (setq on-dialog-closed
        (lambda ()
            (dialog "He seems harmless, invite him aboard?")

          (dialog-await-binary-q-w/lore "Welcome aboard!" "Not today."
                                        '(("How'd you get here?" .
                                           "<c:Castaway:1>I was travelling on an airship that got boarded by goblins. I panicked and jumped in a transporter. I guess things could have turned out much worse, heh. <B:0> Wanna join up?")))

          (setq on-dialog-closed '())))

  (setq on-converge nil))

(defn on-dialog-accepted ()
  (let ((temp (chr-slots (player)))
        (join (lambda (txt)
                  (adventure-log-add 7 '())
                (dialog txt))))
    (if temp
        (progn
          (setq temp (sample temp))
          (chr-new (player) (car temp) (cdr temp) 'neutral '((race . 0) (icon . 1)))
          (chr-del (opponent) 1 14)
          (if (or (chance 2) (< (coins) 300))
              (join "The castaway joined your crew!")
              (progn
                (coins-set (- (coins) 300))
                (join "The castaway joined your crew. Starving, he ate 300@ of your food supplies!"))))
        (progn
          (dialog "Sadly, there's no room...")
          (defn on-dialog-closed ()
            (dialog "<c:Castaway:1>Hold on, don't leave me here! I may not meet anyone else for a long time... I'll help you build an addition onto your castle, then there'll be enough space for me to sleep! Let's see... I've got just enough supplies to build a ladder...")
            (defn on-dialog-closed ()
              (alloc-space 'ladder)
              (sel-input 'ladder
                         "Place ladder (1x2):"
                         (lambda (isle x y)
                             (sound "build0")
                           (room-new (player) `(ladder ,x ,y))
                           (chr-del (opponent) 1 14)
                           (chr-new (player) x (+ 1 y) 'neutral '((race . 0) (icon . 1)))
                           (dialog "<c:Castaway:1> Thanks for rescuing me! I'll try to help out however I can!")
                           (defn on-dialog-closed ()
                             (join "The castaway joined your crew!")
                             (defn on-dialog-closed ()
                               (exit-with-commentary "welcomes_castaway_1"))))))))))
  (exit))


(setq on-dialog-declined exit)
