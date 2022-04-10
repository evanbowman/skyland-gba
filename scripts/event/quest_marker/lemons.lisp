

(dialog "Farmer Meyer's brother waves in the distance, excited to see you! Err... hopefully you managed to keep some of his lemon trees alive...")



(opponent-init 6 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (coconut-palm 5 13)))


(setq on-converge
      (lambda
        (if (not (bound 'lemon-quest-max-reward))
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

              (coins-add c)

              (unbind 'lemon-quest-max-reward)
              (setq quests '())

              (map
               (lambda
                 (if (equal (car $0) 'lemon-tree)
                     (room-rem (player) (get $0 1) (get $0 2))))
               (rooms (player)))

              (map
               (lambda
                 ((room-new
                   (opponent)
                   (list 'lemon-tree (car $0) (cdr $0)))))
               (construction-sites (opponent) '(1 . 2)))

              ;; For the lemon-tree achievement
              (achieve 14)

              (setq on-dialog-closed exit))))))
