;;;
;;; quest/human/8.lisp
;;;


(dialog
 "A proximity alarm wakes your crew in the night! <B:0>"
 "<b:/scripts/data/img/night.img.bin>"
 "An unknown island appears in the moonlight. <B:0>"
 "You almost ran right into it! Upon contacting them, you find the inhabitants in a heated discussion...")

(weather-set weather-id-night)

(opponent-init 10 'neutral)

(island-configure
 (opponent)
 '((shrubbery 0 14) (bronze-hull 1 13) (shrubbery 1 12) (masonry 1 14 0) (masonry 2 14 0) (power-core 2 11) (masonry 3 14 0) (power-core 4 8) (power-core 4 12) (masonry 4 14 0) (power-core 4 10) (masonry 5 14 0) (masonry 6 14 0) (reactor 6 10) (masonry 6 13 0) (windmill 7 14) (masonry 7 13 0) (masonry 8 14 0) (masonry 8 13 0) (bronze-hull 8 12) (lemon-tree 8 10) (shrubbery 9 14)))


(defn on-converge ()
  (dialog
   "<c:King of Emsshaw Cay:27>As this storm approaches, we keep getting horrible transmissions from islands that fall into the bad weather. <B:0> We're debating what to do with our kingdom's arsenal of atomic weapons... <B:0> Even though they're disabled, we wouldn't want them to fall into the wrong hands. <B:0> Can you go retrieve our stash of deactivated atomics and keep them safe?")

   (dialog-await-binary-q-w/lore "Okay..." "Sorry, but no."
                                 '(("Atomics?" .
                                    "<c:King of Emsshaw Cay:27> During the surface wars, ancient civilizations fought each other with powerful atomic weapons. <B:0> When our ancestors moved up here, they stashed their arsenal in a hidden place for safe keeping. <B:0> Can you go retrieve them?")))

   (defn on-dialog-accepted ()
     (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
       (setq on-dialog-closed exit)
       (if m
           (progn
             (adventure-log-add 63 '())
             (push 'qids 8)
             (push 'quests (cons "atomics.lisp" m))
             (dialog "<c:King of Emsshaw Cay:27> Great! I've marked the location on your sky chart with an *! <B:0> My daughter will go along to oversee things...")
             (defn on-dialog-closed ()
               (dialog "<c:Warrior Princess of E. Cay:28> I'm going too! Better than sitting around here doing nothing... <B:0> Don't worry, I can pull my own weight!")

               (defn on-dialog-closed ()
                 (setq on-dialog-closed nil)
                 (run-util-script
                  "find-crew-slot"
                  "<c:Warrior Princess of E. Cay:28> Hmm... you seem to be out of space... <B:0> Let me fix that!"
                  'ladder
                  "Place block (1x2):"
                  (lambda (x y _)
                    (chr-new (player) x y 'neutral '((icon . 28)))
                    (dialog "The princess joined your crew!")
                    (defn on-dialog-closed ()
                      (exit-with-commentary "welcomes_warrior_princess")))))))
           (progn
             (dialog "<c:King of Emsshaw Cay:27> Hmm, looking at the sky chart, it seems that, unfortunately, you won't be able to make it there in time...")))))


   (setq on-dialog-declined exit))
