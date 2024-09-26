;;;
;;; neutral/2/6_1.lisp
;;;


(dialog
 "A proximity alarm wakes your crew in the night! <B:0>"
 "<b:/scripts/data/img/night.img.bin>"
 "An unknown island appears in the moonlight. <B:0>"
 "You almost ran right into it! Upon contacting them, you find the inhabitants in a heated discussion...")

(weather 7)

(opponent-init 10 'neutral)

(island-configure
 (opponent)
 '((shrubbery 0 14) (bronze-hull 1 13) (shrubbery 1 12) (masonry 1 14 0) (masonry 2 14 0) (power-core 2 11) (masonry 3 14 0) (power-core 4 8) (power-core 4 12) (masonry 4 14 0) (power-core 4 10) (masonry 5 14 0) (masonry 6 14 0) (reactor 6 10) (masonry 6 13 0) (windmill 7 14) (masonry 7 13 0) (masonry 8 14 0) (masonry 8 13 0) (bronze-hull 8 12) (lemon-tree 8 10) (shrubbery 9 14)))


(defn on-converge ()
  (dialog
   "<c:king:27>As this storm approaches, we keep getting horrible transmissions from islands that fall into the bad weather. <B:0> We're debating what to do with our kingdom's arsenal of atomic weapons... <B:0> We wouldn't want them to fall into the wrong hands. <B:0> We've heard good news about your battles with the goblin horde! Can you go retrieve our stash of deactivated atomics and keep them safe?")

   (dialog-await-binary-q-w/lore "okay..." "sorry, but no"
                                 '(("atomics?" .
                                    "<c:king:27> During the surface wars, ancient civilizations fought each other with powerful atomic weapons. <B:0> The fighting left the world scarred and irradiated, and those who remained down below were mutated into vicious goblins. <B:0> When our ancestors moved up here, they stashed their arsenal in a hidden place for safe keeping. <B:0> Can you go retrieve them?")))

   (defn on-dialog-accepted ()
     (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
       (setq on-dialog-closed exit)
       (if m
           (progn
             ;(adventure-log-add 17 '())
             (push 'qids 8)
             (push 'quests (cons "/scripts/event/quest_marker/atomics.lisp" m))
             (dialog "<c:king:27> Great! I've marked the location on your sky chart with an *! <B:0> My son will go along to oversee things...")
             (defn on-dialog-closed ()
               (dialog "The king's son joined your crew!")
               (let ((vec (chr-slots (player)))
                     (slot nil))
                 (when (not vec)
                   (alloc-space 'ladder)
                   (let ((s (construction-sites (player) '(1 . 2))))
                     (room-new (player) `(ladder ,(caar s) ,(cdr (car s))))
                     (setq vec (chr-slots (player)))))
                 (setq slot (car vec))
                 (chr-new (player) (car slot) (cdr slot) 'neutral nil))
               (setq on-dialog-closed exit)))
           (progn
             (dialog "<c:king:27> Hmm, looking at the sky chart, it seems that, unfortunately, you won't be able to make it there in time...")))))


   (setq on-dialog-declined exit))
