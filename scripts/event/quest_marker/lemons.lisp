

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
      (lambda
        (if (not (bound? 'lemon-quest-max-reward))
            (setq lemon-quest-max-reward 99999))

        (let ((c (min (list lemon-quest-max-reward
                            (* 1400
                               (length (filter
                                        (lambda (equal (car $0) 'lemon-tree))
                                        (rooms (player)))))))))
          (if (equal c 0)
              (progn
                (dialog "<c:Farmer Ted:9>Hey, you lost my brother's trees!?")
                (setq on-dialog-closed exit))
            (progn

              (dialog
               "<c:Farmer Ted:9>Wonderful! Here's "
               (string c)
               "@ for your trouble!")

              (adventure-log-add 23 (list c (rcnt (player) 'lemon-tree)))

              (coins-add c)

              (unbind 'lemon-quest-max-reward)
              (setq quests '())

              (map
               (lambda
                 (if (equal (car $0) 'lemon-tree)
                     (room-del (player) (get $0 1) (get $0 2))))
               (rooms (player)))

              (map
               (lambda
                 (room-new
                  (opponent)
                  (list 'lemon-tree (car $0) (cdr $0))))
               (construction-sites (opponent) '(1 . 2)))

              ;; For the lemon-tree achievement
              (achieve 14)

              (setq on-dialog-closed exit)
              (setq on-converge nil))))))
