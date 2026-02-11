;;;
;;; neutral/0/5_human.lisp
;;;


(dialog
 "A distress call sounds over your radio! <B:0> "
 "<b:/scripts/data/img/destroyed_town.img.bin>"
 "The remnants of a town appear, wrecked by war..")

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


(chr-new (opponent) 1 12 'neutral 0)


(defn on-converge ()
  (setq on-converge nil)
  (await (dialog* "<c:Girl:14>Heya! I'm so lucky someone showed up! Damned goblins took my whole village as hostages. Somehow I slept through the whole thing... Anyway, please take me with you! I promise not to get in the way!"))

  (if (dialog-await-binary-q-w/lore "She seems harmless, invite her aboard?"
                                    "Sure!"
                                    "No."
                                    '(("Why let her aboard?" .
                                       "<c:Girl:14>Hmm.. well let's see. I'm good at cooking, I can clean your ship, I'm pretty decent at crochet and... I don't know if this is even relevant, but I'm a blackbelt in karate! <B:0> Can I come along?")))
      (on-dialog-accepted)
      (on-dialog-declined)))


(defn/temp join-crew (xy messages)
  (chr-del (opponent) 1 12)
  (chr-new (player) (car xy) (cdr xy) 'neutral '((icon . 14)))
  (adventure-log-add 15 '())
  (apply dialog-sequence messages)
  (pickup-cart 2 "<c:Girl:14>.<d:500>.<d:500>.<d:500> Actually, I was wondering if you can do me one more small favor? I brought this data cartridge with an old photo of my village, can you hold onto it for me?")
  (exit-with-commentary "welcomes_girl"))


(defn on-dialog-accepted ()
  (let ((slots (chr-slots (player))))
    (if slots
        (join-crew (sample slots) '("The villager girl joined your crew!"))
        (progn
          (await (dialog* "Sadly, there's no room..."))
          (await (dialog* "<c:Girl:14>Wait up a second, I know your castle's pretty full, but don't leave me here! This island is literally burning! I'll even sleep in a cargo bay..."))
          (alloc-space 'cargo-bay)
          (let (((x . y) (await (sel-input* 'cargo-bay "Place cargo bay (1x2):"))))
            (sound "build0")
            (room-new (player) `(cargo-bay ,x ,y))
            (join-crew (cons x (incr y)) ; slot in cargo bay is y+1
                       '("<c:Girl:14>Wait, you're serious! I guess I asked for it haha..."
                         "The villager girl joined your crew!")))))))


(setq on-dialog-declined exit)
