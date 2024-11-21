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
  (dialog
   "<c:castaway:27>Argh. I'm in a tough spot here. After years of fighting goblins, my crew "
   "mutinied and left me stranded here. <B:0> At least they picked an island with food, but I'm "
   "getting so fed up with eating coconuts and bananas!")

  (setq on-dialog-closed
        (lambda ()
            (dialog "He seems harmless, invite him aboard?")
          (dialog-await-y/n)
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
          (chr-new (player) (car temp) (cdr temp) '((race . 0) (icon . 1)))
          (chr-del (opponent) 1 14)
          (join "The castaway joined your crew!"))
        (progn
          (dialog "Sadly, there's no room...")
          (defn on-dialog-closed ()
            (dialog "<c:castaway:27>Hold on, don't leave me here! I may not meet anyone else for a long time... I'll help you build an addition onto your castle, then there'll be enough space for me to sleep! Let's see... I've got just enough supplies to build a ladder...")
            (defn on-dialog-closed ()
              (alloc-space 'ladder)
              (sel-input 'ladder
                         "Place ladder (1x2):"
                         (lambda (isle x y)
                             (sound "build0")
                           (room-new (player) `(ladder ,x ,y))
                           (chr-del (opponent) 1 14)
                           (chr-new (player) x (+ 1 y) '((race . 0) (icon . 1)))
                           (dialog "<c:castaway:27> Thanks for rescuing me! I'll try to help out however I can!")
                           (defn on-dialog-closed ()
                             (join "The castaway joined your crew!")
                             (setq on-dialog-closed nil)
                             (exit)))))))))
  (exit))


(setq on-dialog-declined exit)
