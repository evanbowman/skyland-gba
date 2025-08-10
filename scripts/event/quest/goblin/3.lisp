;;;
;;; quest/goblin/3.lisp
;;;


(dialog
 "<b:/scripts/data/img/flares.img.bin>"
 "In the distance, you spot a series of colored smoke signals rising from several islands...")


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
   "<c:Goblin Scout:38>Ssseeing your flag on the horizon givesss us hope! <B:0> Our clan was ssscattered when the ssstorm changed course. We're too few to sssurvive alone, but together... <B:0> Help gather my crew, and we'll join your ssstrength with oursss!")

  (dialog-await-binary-q "I accept!" "I'm kind of busy…"))


(defn on-dialog-accepted ()
  (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
    (setq on-dialog-closed exit)
    (if m
        (progn
          (push 'qids 3)
          (push 'quests (cons "goblin_pickup.lisp" m))
          (adventure-log-add 19 '())
          (dialog "<c:Goblin Scout:38>Yesss! I've marked their sssignals on your chart with an *!"))
      (progn
        (dialog "<c:Goblin Scout:38>The ssstorm movesss too fast... we won't reach them in time. My clan isss lossst...")))))


(defn on-dialog-declined ()
  (dialog "<c:Goblin Scout:38>Fine then... we'll find another way...")
  (setq on-dialog-closed exit))
