;;;
;;; quest/goblin/0.lisp
;;;


(dialog
 "<b:/scripts/data/img/raider_waypoint.img.bin>"
 "You approach a raider waypoint. A goblin crew broadcasts a signal requesting help...")


(opponent-init 9 'neutral)

(island-configure
 (opponent)
 '((hull 0 11 60) (cannon 0 14) (cannon 0 12) (hull 0 13 60) (hull 1 9 60) (hull 1 14) (hull 1 10) (hull 1 11 60) (hull 1 13) (hull 1 12 60) (hull 2 9 60) (workshop 2 10) (masonry 2 14 0) (workshop 2 12) (masonry 3 14 3) (chaos-core 4 12) (hull 4 9) (power-core 4 10) (missile-silo 5 8) (plundered-room 6 10) (plundered-room 6 12) (masonry 6 14 2) (missile-silo 6 8) (crane 7 11) (masonry 7 14 0) (masonry 7 13 0) (hull 7 10) (shrubbery 7 9) (hull 8 14) (hull 8 13) (hull 8 10) (hull 9 14) (banana-plant 9 10)))

(flag-show (opponent) flag-id-pirate)


(setq on-converge
      (lambda ()
        (dialog
         "<c:Raider:39>Sssnatched some valuable cargo from a merchant fleet, but our ship took heavy damage in the fight. With that ssstorm approaching, we won't make it to the black market in time. Help usss deliver the goods? We'll sssplit the profit, and our contact paysss well!")
        (dialog-await-binary-q "I accept!" "Sorry, but no.")

        (setq on-dialog-accepted
              (lambda ()
                (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
                  (if m
                      (run-util-script
                       "find-or-create-cargo-bay"
                       (lambda (x y)
                         (push 'quests (cons "loot.lisp" m))
                         (coins-add 500)
                         (push 'qids 0)
                         (adventure-log-add 16 '())
                         (cargo-set (player) x y "loot")
                         (dialog "<c:raider:39>Excccellent! I'll mark the location with an * on your chart!")
                         (setq on-dialog-closed exit)))
                      (progn
                        (dialog
                         "<c:Raider:39>Bah! Jussst got word our contact had to move their "
                         "operation - ssstorm's getting too close. Here'sss 400@ for your time, "
                         "at leassst.")
                        (setq on-dialog-closed
                              (lambda ()
                                (coins-add 400)
                                (exit))))))))

        (setq on-dialog-declined
              (lambda ()
                (dialog "<c:Raider:39>Bah, fine... We'll find another sssship to move the loot...")
                (setq on-dialog-closed exit)))))
