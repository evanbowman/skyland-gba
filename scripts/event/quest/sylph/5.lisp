;;;
;;; quest/human/5.lisp
;;;


(dialog
 "<b:/scripts/data/img/wanderer_town.img.bin>"
 "While out on reconnaissance, your crew discovers an overcrowded transit hub, full of refugees departing the storm... <B:0> "
 "<b:/scripts/data/img/wanderer.img.bin> In the local cantina, a mysterious traveller offers you a deal... <B:0> You change course to dock with the station...")


(opponent-init 14 'neutral)

(island-configure
 (opponent)
 '((power-core 0 12)
   (masonry 0 14 3)
   (workshop 1 6)
   (masonry 1 14 3)
   (masonry 2 13 0)
   (masonry 2 8 3)
   (masonry 2 14 3)
   (workshop 3 5)
   (masonry 3 8 3)
   (masonry 3 13 2)
   (masonry 3 10 3)
   (masonry 3 14 3)
   (windmill 3 9)
   (masonry 3 7 3)
   (workshop 3 11)
   (masonry 4 14 3)
   (masonry 4 13 3)
   (masonry 5 14 0)
   (bridge 5 12)
   (shrubbery 5 10)
   (hull 5 11)
   (masonry 5 13 0)
   (bridge 5 6)
   (masonry 6 14 0)
   (lemon-tree 7 9)
   (bridge 7 12)
   (bridge 7 6)
   (masonry 7 14 3)
   (hull 7 11)
   (masonry 7 13 3)
   (bridge 9 6)
   (hull 9 11)
   (masonry 9 14 0)
   (statue 9 9 3)
   (bridge 9 12)
   (hull 10 11)
   (shrubbery 10 10)
   (masonry 10 14 0)
   (masonry 10 13 0)
   (workshop 11 5)
   (transporter 11 11)
   (masonry 11 14 3)
   (masonry 11 13 0)
   (masonry 12 10 3)
   (masonry 12 9 3)
   (masonry 12 8 3)
   (masonry 12 7 3)
   (transporter 12 11)
   (masonry 12 14 3)
   (masonry 12 13 0)
   (masonry 13 14 3)
   (masonry 13 13 0)
   (stairwell 13 9)
   (stairwell 13 5)))


(defn on-converge ()
  (setq on-converge nil)

  (dialog "<c:Traveller:23> Hello! I was just talking to your crew! A few weeks ago, some wretched goblins ransacked my island. Outnumbered, I jumped in a transporter and ended up here. Can you help me get back home?")

  (dialog-await-binary-q "Of course!" "I'm kind of busy…")

  (defn on-dialog-accepted ()
    (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
      (when m
        (push 'qids 5)
        (adventure-log-add 52 '())
        (push 'quests (cons "traveller.lisp" m)))

      (run-util-script
       "find-crew-slot"
       "<c:Traveller:23>Oh! It looks like you're out of room! Let me fix that..."
       'ladder
       "Place block (1x2):"
       (lambda (x y _)
         (let ((id (chr-new (player) x y 'neutral '((icon . 23)))))
           (push 'qvar (cons 5 id)))

         (dialog (if m
                     "<c:Traveller:23>Great! I'll come aboard and travel to the destination with you! I've marked the location on your sky chart with an *..."
                     "<c:Traveller:23>Looking at your sky chart, doesn't seem like we can get there before the storm overtakes us. I'll join your crew anyway, better than waiting in line for a transporter here..."))
         (defn on-dialog-closed ()
           (dialog "The mysterious traveller joined your crew!")
           (defn on-dialog-closed ()
             (exit-with-commentary "welcomes_traveller")))))))


  (defn on-dialog-declined ()
    (exit)))
