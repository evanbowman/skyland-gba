;;;
;;; quest/goblin/8.lisp
;;;


(dialog
 "A proximity alarm wakes your crew in the night! <B:0>"
 "<b:/scripts/data/img/night.img.bin>"
 "An ancient fortress appears in the moonlight. <B:0>"
 "You almost ran right into it! Inside, you hear the sounds of ritual chanting...")

(weather-set 7)

(opponent-init 10 'neutral)

(island-configure
 (opponent)
 '((shrubbery 0 14) (bronze-hull 1 13) (shrubbery 1 12) (masonry 1 14 0) (masonry 2 14 0) (power-core 2 11) (masonry 3 14 0) (power-core 4 8) (power-core 4 12) (masonry 4 14 0) (power-core 4 10) (masonry 5 14 0) (masonry 6 14 0) (reactor 6 10) (masonry 6 13 0) (windmill 7 14) (masonry 7 13 0) (masonry 8 14 0) (masonry 8 13 0) (bronze-hull 8 12) (lemon-tree 8 10) (shrubbery 9 14)))


(defn on-converge ()
  (dialog
   "<c:Ashwalker Elder:42>The ssstorm brings change... Our seers have had visionsss of an ancient weapon, buried in the ruinsss of our ancestorsss. <B:0> A relic from before the Great Change that made usss what we are... <B:0> It must not remain below when the ssstorm arrives. <B:0> Will you help retrieve it?")

   (dialog-await-binary-q "Okay..." "Sssorry, but no.")

   (defn on-dialog-accepted ()
     (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
       (setq on-dialog-closed exit)
       (if m
           (progn
             (adventure-log-add 63 '())
             (push 'qids 8)
             (push 'quests (cons "goblin_atomics.lisp" m))
             (dialog "<c:Ashwalker Elder:42> The location hasss been marked on your chart with an *! <B:0> My apprentice will guide you through the old rituals...")
             (defn on-dialog-closed ()
               (dialog "<c:Ashwalker Apprentice:41> Finally, a chance to prove my worth to the Order! <B:0> I know all the ancient wardsss against radiation!")

               (defn on-dialog-closed ()
                 (setq on-dialog-closed nil)
                 (run-util-script
                  "find-crew-slot"
                  "<c:Ashwalker Apprentice:41> Hmm... you ssseem to be out of ssspace... <B:0> Let me fix that!"
                  'ladder
                  "Place block (1x2):"
                  (lambda (x y _)
                    (chr-new (player) x y 'neutral '((icon . 41) (race . 1)))
                    (dialog "The apprentice joined your crew!")
                    (setq on-dialog-closed exit))))))
           (progn
             (dialog "<c:Ashwalker Elder:42>The visionsss were unclear... The ssstorm movesss too quickly, the path isss already blocked...")))))


   (setq on-dialog-declined exit))
