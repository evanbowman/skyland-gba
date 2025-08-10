;;;
;;; quest_marker/lemons.lisp
;;;


(dialog "Farmer Meyer's brother waves in the distance, excited to see you! Err... hopefully you managed to keep some of his lemon trees alive...")



(opponent-init 9 'neutral)

(island-configure
 (opponent)
 '((bronze-hull 0 13)
   (bronze-hull 0 12)
   (bronze-hull 0 11)
   (bronze-hull 0 14)
   (masonry 1 13)
   (water-source 1 14)
   (masonry 2 13)
   (masonry 2 14)
   (water-source 3 14)
   (masonry 3 13)
   (masonry 4 14)
   (masonry 4 13)
   (masonry 5 13)
   (water-source 5 14)
   (power-core 6 13)
   (bronze-hull 6 12)
   (bronze-hull 7 12)
   (workshop 7 10)
   (bronze-hull 7 9)
   (bronze-hull 8 9)))


(setq on-converge
      (lambda ()
        (let ((c (min (list (lookup 2 qvar)
                            (* 1400
                               (length (filter
                                        (lambda (room)
                                          (equal (car room) 'lemon-tree))
                                        (rooms (player)))))))))
          (if (equal c 0)
              (progn
                (dialog 
                (if (equal (faction) 'goblin)
                      "<c:Farmer Ted:9>From the moment I saw you nasty goblins, I knew there was no hope."
                      "<c:Farmer Ted:9>Hey, you lost my brother's trees!?"))
                (setq on-dialog-closed exit))
            (progn

              (dialog
               "<c:Farmer Ted:9>Whoa! I didn't think there would be any left, Here's "
               (string c)
               "@ for your trouble!")

              (adventure-log-add 23 (list c (room-count (player) 'lemon-tree)))

              (coins-add c)

              (setq quests '())

              (map (lambda (room)
                    (if (equal (car room) 'lemon-tree)
                        (room-del (player) (get room 1) (get room 2))))
                   (rooms (player)))

              (map (lambda (xy)
                     (room-new (opponent)
                               (list 'lemon-tree (first xy) (second xy))))
                   (construction-sites (opponent) '(1 . 2)))

              ;; For the lemon-tree achievement
              (achieve 14)

              (setq on-dialog-closed exit)
              (setq on-converge nil))))))
