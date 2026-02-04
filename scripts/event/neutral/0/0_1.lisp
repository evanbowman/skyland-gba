;;;
;;; neutral/0/0_1.lisp
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
          "<c:Castaway:27>Argh. I'm in a tough spot here. After years of fighting goblins, my crew "
          "mutinied and left me stranded here. <B:0> At least they picked an island with food, but "
          "I'm getting so fed up with eating coconuts and bananas!"))

  (if (dialog-await-y/n "He seems harmless, invite him aboard?")
      (on-dialog-accepted)
      (on-dialog-declined)))


(setq on-dialog-declined exit)


(defn/temp join-crew (xy (msg . string))
  (adventure-log-add 7 '())
  (chr-new (player) (car xy) (cdr xy) 'neutral '((race . 0) (icon . 27)))
  (chr-del (opponent) 1 14)
  (await (dialog* msg)))


(defn/temp join-has-space (slots)
  (join-crew (sample slots) "The castaway joined your crew!")
  (exit-with-commentary "welcomes_castaway_2"))


(defn/temp join-crowded ()
  (await (dialog* "Sadly, there's no room..."))
  (await (dialog* "<c:Castaway:27>Hold on, don't leave me here! I may not meet anyone else for a long time... I'll help you build an addition onto your castle, then there'll be enough space for me to sleep! Let's see... I've got just enough supplies to build a ladder..."))
  (alloc-space 'ladder)
  (let ((xy (await (sel-input* 'ladder "Place ladder (1x2):"))))
    (sound "build0")
    (room-new (player) `(ladder ,(car xy) ,(cdr xy)))
    (chr-del (opponent) 1 14)
    (join-crew xy "<c:Castaway:27> Thanks for rescuing me! I'll try to help out however I can!")
    (await (dialog* "The castaway joined your crew!"))
    (exit-with-commentary "welcomes_castaway_2")))


(defn on-dialog-accepted ()
  (let ((slots (chr-slots (player)))
        (join (lambda (txt)
                (adventure-log-add 7 '())
                (dialog txt))))
    (if slots
        (join-has-space slots)
        (join-crowded))))
