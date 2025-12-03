;;;
;;; quest/human/3.lisp
;;;


(dialog
 "<b:/scripts/data/img/airship.img.bin>"
 "While passing by a small airship, a the owner frantically signals you with his radio...")


(opponent-init 6 'neutral)

(island-configure
 (opponent)
 '((balloon 0 7)
   (bronze-hull 0 12)
   (infirmary 0 13)
   (bronze-hull 1 12)
   (bronze-hull 2 12)
   (bridge 2 14)
   (balloon 3 7)
   (bronze-hull 3 12)
   (bronze-hull 4 12)
   (workshop 4 13)
   (bronze-hull 5 12)))


(defn on-converge ()
  (dialog
   "<c:Chauffeur:11>Hey man, I was supposed to pick up a whole bunch of people and give em a lift outta here, but the skies've been crawling with goblins ever since these storm clouds started rollin' in, I don't think I can make it there in time. You've got a big, powerful fortress, wanna help a fella out?")

  (dialog-await-binary-q-w/lore "I accept!" "I'm kind of busy…"
                                '(("Explain transit more…" .
                                   "<c:Chauffeur:11>Ah! I thought you would already know, but I can explain that more. Most medium distance transit takes place using transporters. But over long distances, warp transit can be a bit unreliable. <B:0> (You wouldn't want to end up inside a wall, or worse) <B:0> So long distance transport takes place using airships. Many isles are propelled by powerful atomic reactors, but it doesn't make sense to use that much energy for passenger transit, so my airship is slower and powered by balloon. <B:0> Anyway, can you help pick up these people?"))))


(defn on-dialog-accepted ()
  (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
    (setq on-dialog-closed exit)
    (if m
        (progn
          (push 'qids 3)
          (push 'quests (cons "pickup.lisp" m))
          (adventure-log-add 19 '())
          (dialog "<c:Chauffeur:11>Thanks a lot! I just marked the location your sky chart with an *!"))
      (progn
        (dialog "<c:Chauffeur:11>Ya know, now that I look at this map, I don't think you can make it there in time either, I hope they won't get too mad at me...")))))


(defn on-dialog-declined ()
  (dialog "<c:Chauffeur:11>Hey, don't worry, I understand...")
  (setq on-dialog-closed exit))
