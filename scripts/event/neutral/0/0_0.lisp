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
  (setq on-converge nil)
  (await (dialog*
          "<c:Castaway:1>Fancy meeting you here! I've been marooned on this island... "
          "who knows how long! Looks like a nasty storm's brewing, mind "
          "if I hitch a ride?"))

  (if (dialog-await-binary-q-lore "He seems harmless, invite him aboard?"
                                  "Welcome aboard!"
                                  "Not today."
                                  '(("How'd you get here?" . "<c:Castaway:1>I was travelling on an airship that got boarded by goblins. I panicked and jumped in a transporter. I guess things could have turned out much worse, heh. <B:0> Wanna join up?")))
      (on-dialog-accepted)
      (on-dialog-declined)))



(defn/temp join-crew (xy (msg . string))
  (adventure-log-add 7 '())
  (chr-new (player) (car xy) (cdr xy) 'neutral '((race . 0) (icon . 1)))
  (chr-del (opponent) 1 14)
  (await (dialog* msg)))



(defn/temp join-has-space (slots)
  (if (or (chance 2) (< (coins) 300))
      (join-crew (sample slots)
                 "The castaway joined your crew!")
      (progn
        (coins-set (- (coins) 300))
        (join-crew (sample slots)
                   "The castaway joined your crew. Starving, he ate 300@ of your food supplies!")))
  (exit-with-commentary "welcomes_castaway_1"))



(defn/temp join-crowded ()
  (await (dialog* "Sadly, there's no room..."))
  (await (dialog* "<c:Castaway:1>Hold on, don't leave me here! I may not meet anyone else for a long time... I'll help you build an addition onto your castle, then there'll be enough space for me to sleep! Let's see... I've got just enough supplies to build a ladder..."))
  (alloc-space 'ladder)
  (let ((xy (await (sel-input* 'ladder "Place ladder (1x2):"))))
    (sound "build0")
    (room-new (player) `(ladder ,(car xy) ,(cdr xy)))
    (join-crew xy "<c:Castaway:1> Thanks for rescuing me! I'll try to help out however I can!")
    (await (dialog* "The castaway joined your crew!"))
    (exit-with-commentary "welcomes_castaway_1")))



(defn on-dialog-accepted ()
  (let ((slots (chr-slots (player))))
    (if slots
        (join-has-space slots)
        (join-crowded))))



(defn on-dialog-declined ()
  (exit))
