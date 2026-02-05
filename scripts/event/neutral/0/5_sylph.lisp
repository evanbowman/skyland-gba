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
   "<c:Sylph Researcher:49>Someone's here! Thank the winds. <B:0> I was conducting field research when raiders attacked. I tried to warn the villagers - they thought I was overreacting. 'Sylph paranoia,' one said. <B:0> When the attack came, I tried to help defend, but... <B:0> I'm a researcher, not a soldier. The building collapsed. Everyone scattered or was taken. <B:0> By the time I dug myself out, I was alone.")
  (setq on-dialog-closed
        (lambda ()
          (dialog "He seems shaken, invite him aboard?")
          (dialog-setup-binary-q-w/lore "Welcome aboard!" "Not today."
                                        '(("field research?" .
                                           "<c:Sylph Researcher:49>We document adaptation strategies - how humans build efficiently, manage with limited resources. <B:0> The Conclave thought understanding survival methods might help our cities. <B:0> <d:800> I told them their defenses were inadequate. They didn't listen. <B:0> Maybe I didn't explain it right. Maybe they thought I was... condescending.")))
          (setq on-dialog-closed '())))
  (setq on-converge nil))


(defn on-dialog-accepted ()
  (find-crew-slot-cb
   "<c:Sylph Researcher:49>Ah. Full capacity. That's... let me think. <B:0> The structural integrity here is compromised. I can calculate optimal placement for-"
   'ladder
   "Place block (1x2):"
   (lambda (x y _)
     (chr-del (opponent) 1 12)
     (chr-new (player) x y 'neutral '((race . 4) (icon . 49)))
     (dialog "<c:Sylph Researcher:49>Thank you. <B:0> <d:600> I keep thinking - if I'd phrased it differently, shown them the calculations in a way they'd understand... <B:0> <d:800> But I presented it like a research paper. To people fighting for survival every day. <B:0> They needed practical help, not observations.")
     (adventure-log-add 15 '())
     (defn on-dialog-closed ()
       (setq on-dialog-closed nil)
       (dialog "The Sylph joined your crew!")
       (exit)))))


(setq on-dialog-declined exit)
