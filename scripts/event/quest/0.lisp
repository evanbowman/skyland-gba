

(dialog
 "You arrive at a busy trading outpost. "
 "Some merchants broadcast a message asking for assistance...")


(opponent-init 6 'neutral)

(island-configure
 (opponent)
 '((bronze-hull 0 11)
   (bronze-hull 0 10)
   (hull 0 9)
   (statue 0 7)
   (bronze-hull 0 14)
   (bronze-hull 0 12)
   (bronze-hull 0 13)
   (workshop 1 11)
   (bridge 1 10)
   (masonry 1 14)
   (masonry 1 13)
   (masonry 2 13)
   (masonry 2 14)
   (stairwell 3 11)
   (bridge 3 10)
   (shrubbery 4 13)
   (masonry 4 14)
   (bridge 5 10)
   (masonry 5 13)
   (masonry 5 12)
   (masonry 5 11)
   (masonry 5 14)
   (bronze-hull 5 9)
   (banana-plant 5 8)
   (power-core 6 13)
   (masonry 7 12)
   (masonry 7 11)
   (bridge 7 10)
   (bronze-hull 7 9)
   (banana-plant 7 8)
   (stairwell 8 11)
   (shrubbery 8 9)))


(setq on-converge
      (lambda
        (dialog
         "<c:merchant:7>We promised to deliver some cargo to our customers, but with "
         "this storm approaching, we don't think we can make the delivery. "
         "Can you help? We'll pay you a bit upfront, and I'm sure that they'll tip "
         "you generously.")
        (dialog-await-y/n)

        (setq on-dialog-accepted
              (lambda
                (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp"))
                      (c (eval-file "/scripts/util/find_create_cargo_bay.lisp")))
                  (if (and m c)
                      (progn
                        (coins-add 500)
                        (push 'quests (cons "/scripts/event/quest_marker/delivery.lisp"
                                            m))
                        (cargo-set (player) (car c) (cdr c) "parcel")
                        (dialog "<c:merchant:7>Wonderful! I'll mark the address "
                                "with an * on your sky chart!")
                        (setq on-dialog-closed exit))
                    (progn
                      (dialog
                       "<c:merchant:7>Oh, I'm so sorry! I just got a call from the customer, "
                       "she had to relocate to flee the storm. Here's 400@ for your trouble.")
                      (setq on-dialog-closed
                            (lambda
                              (coins-add 400)
                              (exit))))))))

        (setq on-dialog-declined
              (lambda
                (dialog "<c:merchant:7>I understand... I guess we'll try to find someone else...")
                (setq on-dialog-closed exit)))))
