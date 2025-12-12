;;;
;;; neutral/0/5_sylph.lisp
;;;

(dialog
 "A distress call sounds over your radio! <B:0> "
 "<b:/scripts/data/img/destroyed_town.img.bin>"
 "The remnants of a town appear, wrecked by raiders...")


(opponent-init 8 'neutral)

(island-configure
 (opponent)
 '((hull 0 14)
   (hull 0 13)
   (masonry 0 12)
   (torch 0 10)
   (masonry 1 14)
   (masonry 1 13)
   (masonry 2 14)
   (plundered-room 2 12)
   (torch 3 9)
   (masonry 3 14)
   (plundered-room 3 12)
   (masonry 4 14)
   (power-core 5 13)))


(defn on-fadein ()
  (fire-new (opponent) 3 9)
  (fire-new (opponent) 0 10)
  (setq on-fadein nil))


(flag-show (opponent) flag-id-colonist)


(chr-new (opponent) 1 12 'neutral '((race . 4)))


(defn on-converge ()
  (dialog
   "<c:Sylph Researcher:49>Contact! Finally! <B:0> #clears throat# My apologies. Composure. <B:0> I was here conducting field research into human settlements when raiders attacked! <B:0> I tried to stay hidden, but the building I was in collapsed. By the time I dug myself out, everyone was gone - the villagers, the raiders, everyone.")

  (setq on-dialog-closed
        (lambda ()
          (dialog "He seems shaken, invite him aboard?")
          (dialog-await-binary-q-w/lore "Welcome aboard!" "Not today."
                                        '(("field research?" .
                                           "<c:Sylph Researcher:49> We've been documenting how other sky peoples adapt - construction techniques, resource management, social structures. <B:0> The Conclave believes if we understand how humans survive with less... sophisticated technology, we might find solutions to our own problems. <B:0> Observational study only, of course. We don't interfere.")))
          (setq on-dialog-closed '())))

  (setq on-converge nil))


(defn on-dialog-accepted ()
  (run-util-script
   "find-crew-slot"
   "<c:Sylph Researcher:49>Wait - you're full. Let me help. I can't stay here, the fires are spreading."
   'ladder
   "Place block (1x2):"
   (lambda (x y _)
     (chr-del (opponent) 1 12)
     (chr-new (player) x y 'neutral '((race . 4) (icon . 49)))
     (dialog "<c:Sylph Researcher:49>Your assistance is... #breath catches# ...deeply appreciated. <B:0> I will not forget this intervention.")
     (adventure-log-add 15 '())
     (defn on-dialog-closed ()
       (setq on-dialog-closed nil)
       (dialog "The Sylph joined your crew!")
       (exit)))))


(setq on-dialog-declined exit)
