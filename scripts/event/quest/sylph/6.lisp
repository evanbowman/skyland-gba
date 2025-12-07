;;;
;;; quest/sylph/6.lisp
;;;

(dialog "You discover the wreckage of a goblin raid... most of the raiders fled, but among the debris, you hear frightened breathing...")

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


(flag-show (opponent) flag-id-pirate)

(chr-new (opponent) 1 12 'neutral '((race . 1)))

(defn on-converge ()
  (dialog "<c:Goblin Child:50>#hisses# Ssstay away! I know what you are! <B:0> You're... you're Sssylph! <B:0> My father told me! He sssaid if the Sssylph ever catch us, they... they cook goblin children in their big metal potsss! <B:0> #voice cracks# But I won't let you! I'll... I'll bite you!")
  (defn on-dialog-closed ()
    (dialog "The child is terrified. Try to calm him down?")
    (setq on-dialog-closed nil)
    (dialog-await-binary-q-w/lore "Help the child." "Leave him."
                                  '(("we don't eat children" .
                                     "<c:Goblin Child:50>That'sss what you'd sssay! <B:0> Father sssaid you trick us with your fancy wordsss! <B:0> But... but you haven't grabbed me yet... <B:0> Why?")
                                    ("where is your father?" .
                                     "<c:Goblin Child:50>He... he was on one of the other shipsss. <B:0> We were raiding a human town, but their defenssses were too ssstrong. Everyone fled! <B:0> I fell through a hole in the deck. By the time I climbed out... <B:0> #starts crying# Everyone was gone!")))

    (defn on-dialog-accepted ()
      (dialog "<c:Goblin Child:50>You... you'd really take me home? <B:0> But why would Sssylph help a goblin? <B:0> Father sssaid you think we're... what was the word... 'degraded'? That we're not worth sssaving... <B:0> #suspicious# Thisss is a trick, isssn't it?")

      (defn on-dialog-closed ()
        (setq on-dialog-closed nil)
        (let ((sl (chr-slots (player))))
          (when (not sl)
            (alloc-space 'ladder)
            (let ((site (construction-sites (player) '(1 . 2))))
              (sound "build0")
              (room-new (player) `(ladder ,(caar site) ,(cdar site)))))
          (setq sl (chr-slots (player)))
          (let ((id (chr-new (player)
                             (caar sl)
                             (cdar sl)
                             'neutral
                             '((icon . 50)
                               (race . 1)))))
            (chr-hp id 128)
            (chr-del (opponent) 1 12)
            (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
              (if m
                  (progn
                    (adventure-log-add 54 nil)
                    (push 'qids 6)
                    (push 'quests (cons "goblin_child_return.lisp" m))
                    (push 'qvar (cons 6 id))
                    (dialog "<c:Goblin Child:50>#whispers nervously# Don't show fear. Father sssaid they can sssmell fear... <B:0> Wait... where are your cooking pots?")
                    (defn on-dialog-closed ()
                      (dialog "The goblin child joined your crew! He's marked his home settlement on your sky chart with an *...")
                      (setq on-dialog-closed nil)
                      (exit)))
                (progn
                  (dialog "The goblin child joined your crew! Maybe you can find his home...")
                  (exit))))))))

    (setq on-dialog-declined
          (lambda ()
            (dialog "<c:Goblin Child:50>#voice breaking# I knew it... no one comesss back for goblinss... <B:0> You leave him behind in the ruins.")
            (exit)))))
