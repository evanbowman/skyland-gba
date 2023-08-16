
(dialog "<b:/scripts/misc/img/farm_colony.skg>A nearby farming colony requests assistance...")


(opponent-init 8 'neutral)

(island-configure
 (opponent)
 '((bronze-hull 0 13)
   (sunflower 0 11)
   (bronze-hull 0 12)
   (bronze-hull 0 14)
   (power-core 1 13)
   (bronze-hull 1 12)
   (bronze-hull 2 12)
   (bronze-hull 2 11)
   (lemon-tree 2 9)
   (fountain 3 10)
   (lemon-tree 3 13)
   (bronze-hull 3 11)
   (fountain 4 14)
   (lemon-tree 4 9)
   (bronze-hull 4 11)
   (lemon-tree 5 9)
   (lemon-tree 5 13)
   (bronze-hull 5 11)
   (lemon-tree 6 13)
   (bronze-hull 6 11)
   (lemon-tree 6 9)
   (masonry 7 14)
   (masonry 7 13)
   (bronze-hull 7 12)
   (workshop 7 10)
   (masonry 8 14)
   (masonry 8 13)
   (bronze-hull 8 12)))

(secret
 4 11
 "Farms in Skyland grow crops cultivated for cold air. Even so, nothing would grow at this altitude without heat from the island core...")

(defn on-converge
  (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
    (if m
        (progn
          (dialog
           "<c:Farmer Meyer:9>Looks like a bad storm's coming this way! Hey, I have an idea! How about you do me a favor and relocate my orchard of lemon trees to my brother's island... I'll tell him to pay you 1400@ for each one that you successfully deliver!")

          (defn on-dialog-closed
            (map
             (lambda
               (if (equal (car $0) 'lemon-tree)
                   (room-del (opponent) (get $0 1) (get $0 2))))
             (rooms (opponent)))

            ;; In case some player decides to farm coins by building his/her own
            ;; lemon trees and passing them off as Farmer Meyer's trees.
            (setq lemon-quest-max-reward 0)

            (push 'qids 2)
            (push 'quests (cons "/scripts/event/quest_marker/lemons.lisp" m))

            (map
             (lambda
               ((room-new
                 (player)
                 (list 'lemon-tree (car $0) (cdr $0)))
                (+= lemon-quest-max-reward 1400)))
             (construction-sites (player) '(1 . 2)))

            (adventure-log-add 18 (list (rcnt (player) 'lemon-tree)))

            (dialog "<c:Farmer Meyer:9>Please take good care of them! I marked my brother's address on your sky chart with an *!")
            (setq on-dialog-closed exit)))
      (progn
        (dialog "<c:Farmer Meyer:9>I was going to ask you to help relocate some of these lemon trees, but the storm's getting closer and I need to move out! Maybe we'll meet again, someday...")
        (setq on-dialog-closed exit)))))
