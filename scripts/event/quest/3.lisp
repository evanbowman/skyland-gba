
(dialog
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


(defn on-converge [0]
  (dialog
   "<c:chauffeur:11>Hey man, I was supposed to pick up a whole bunch of people and give em a lift outa here, but the skies've been crawling with goblins ever since these storm clouds started rollin' in, I don't think I can make it there in time. You've got a big, powerful fortress, wanna help a fella out?")

  (dialog-await-binary-q "I accept!" "I'm kind of busy…"))


(defn on-dialog-accepted [0]
  (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
    (setq on-dialog-closed exit)
    (if m
        (progn
          (push 'qids 3)
          (push 'quests (cons "/scripts/event/quest_marker/pickup.lisp" m))
          (adventure-log-add 19 '())
          (dialog "<c:chauffeur:11>Thanks a lot! I just marked the location your sky chart with an *!"))
      (progn
        (dialog "<c:chauffeur:11>Ya know, now that I look at this map, I don't think you can make it there in time either, I hope they won't get too mad at me...")))))


(defn on-dialog-declined [0]
  (dialog "<c:chauffeur:11>Hey, don't worry, I understand...")
  (setq on-dialog-closed exit))
