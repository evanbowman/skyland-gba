
(dialog
 "While passing by a small ship, a the owner frantically signals you with his radio...")



(opponent-init 3 'neutral)

(island-configure
 (opponent)
 '((power-core 0 13)))


(defn on-converge
  (dialog
   "<c:chauffeur:10>Hey man, I was supposed to pick up a whole bunch of people and give em a lift outa here, but the skies've been crawling with goblins ever since these storm clouds started rollin' in, I don't think I can make it there in time. You've got a big, powerful fortress, wanna help a fella out?")

  (dialog-await-y/n))


(defn on-dialog-accepted
  (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
    (setq on-dialog-closed exit)
    (if m
        (progn
          (push 'qids 3)
          (push 'quests (cons "/scripts/event/quest_marker/pickup.lisp" m))
          (adventure-log-add 19 '())
          (dialog "<c:chauffeur:10>Thanks a lot! I just marked the location your sky chart with an *!"))
      (progn
        (dialog "<c:chauffeur:10>Ya know, now that I look at this map, I don't think you can make it there in time either, I hope they won't get too mad at me...")))))


(defn on-dialog-declined
  (lambda
    (dialog "<c:chauffeur:10>Hey, don't worry, I understand...")
    (setq on-dialog-closed exit)))
