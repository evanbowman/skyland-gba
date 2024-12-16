;;;
;;; neutral/0/5_goblin.lisp
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

(flag-show (opponent) 7)


(chr-new (opponent) 1 12 'neutral '((race . 1)))


(defn on-converge ()
  (dialog
   "<c:goblin raider:39>Finally, ssomeone answered! <B:0> My raid crew left me behind when the town's defensses were stronger than expected. <B:0> Got knocked out by some debris when their ion cannon hit us... By the time I came to, everyone wasss gone. _bitter_laugh_ Guesss they figured I was dead weight. <B:0> Would you have room for a more... reliable crewmember?")

  (setq on-dialog-closed
        (lambda ()
            (dialog "She seems decent, invite her aboard?")

          (dialog-await-binary-q "welcome aboard!" "not today")

          (setq on-dialog-closed '())))

  (setq on-converge nil))


(defn on-dialog-accepted ()
  (run-util-script
   "find-crew-slot"
   "<c:goblin raider:39>Hold on, don't leave me here! I know your casstle's full, but thiss island is burning!"
   'ladder
   "Place block (1x2):"
   (lambda (x y _)
     (chr-del (opponent) 1 12)
     (chr-new (player) x y 'neutral '((race . 1) (icon . 39)))
     (dialog "<c:goblin raider:39> Thanks for ressscuing me! I'll try to help out however I can!")
     (defn on-dialog-closed ()
       (setq on-dialog-closed nil)
       (dialog "The goblin joined your crew!")
       (exit)))))

(setq on-dialog-declined exit)
