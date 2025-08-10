;;;
;;; quest_marker/dyanmite-ii.lisp
;;;


(dialog "You reach the destination mining platform...")


(opponent-init 12 'neutral)


(island-configure
 (opponent)
 '((hull 0 14)
   (hull 0 13)
   (hull 1 14)
   (power-core 1 10)
   (ladder 1 12)
   (masonry 3 11 0)
   (bridge 3 10)
   (hull 4 9)
   (masonry 4 11 0)
   (masonry 5 11 0)
   (bridge 5 10)
   (hull 5 9)
   (masonry 6 11 0)
   (bridge 7 10)
   (masonry 7 11 0)
   (hull 8 9)
   (masonry 8 11 0)
   (crane 9 8)
   (masonry 9 11 0)
   (bridge 9 10)
   (hull 10 14)
   (hull 10 7)
   (masonry 10 13 0)
   (masonry 10 11 0)
   (masonry 10 12 0)
   (hull 11 14)
   (hull 11 13)))


(defn on-converge ()
  (let ((bloc (if (equal (difficulty) 0)
                  'dynamite
                'dynamite-ii)))
    (let ((pos 2)
          (c (min (list (* 7 1600)
                        (* 1600
                           (length (filter
                                    (lambda (room)
                                      (equal (car room) bloc))
                                    (rooms (player)))))))))
      (if (equal c 0)
          (progn
            (dialog "<c:Mining Chief:20>You lost the explosives along the way? You're lucky to have survived! But sorry, we can't pay you.")
            (setq on-dialog-closed exit))
        (progn

          (dialog "<c:Mining Chief:20>Fantastic! We were a bit worried, but you did great! Here's "
                  (string c)
                  "@ for your trouble!")

          (coins-add c)

          (setq quests '())

          (adventure-log-add 58 (list c))

          (map (lambda (room)
                (when (equal (car room) bloc)
                  (room-del (player) (get room 1) (get room 2))
                  (room-new (opponent) (list bloc pos 14))
                  (+= pos 1)))
               (rooms (player)))

          (setq on-dialog-closed exit)
          (setq on-converge nil))))))
