

(dialog
 "You arrive at a busy trading outpost. "
 "Some merchants broadcast a message asking for assistance...")


(opponent-init 6 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (coconut-palm 5 13)))


(setq on-converge
      (lambda
        (dialog
         "We promised to deliver some cargo to our customers, but with this storm "
         "approaching, we don't think we can make the delivery. Can you help? We'll "
         "pay you a bit upfront, and I'm sure that they'll tip you generously.")
        (dialog-await-y/n)

        (setq on-dialog-accepted
              (lambda
                (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp"))
                      (c (eval-file "/scripts/util/find_create_cargo_bay.lisp")))
                  (if (and m c)
                      (progn
                        (push quests (cons "/scripts/event/quest_marker/delivery.lisp"
                                           m))
                        (cargo-set (player) (car c) (cdr c) "parcel")
                        (dialog "Wonderful! I'll mark the address "
                                "with an * on your sky chart!")
                        (setq on-dialog-closed exit))
                    (progn
                      (dialog
                       "Oh, I'm so sorry! I just got a call from the customer, "
                       "she had to relocate to flee the storm. Here's 400@ for your trouble.")
                      (setq on-dialog-closed
                            (lambda
                              (coins-add 400)
                              (exit))))))))

        (setq on-dialog-declined
              (lambda
                (dialog "I understand... I guess we'll try to find someone else...")
                (setq on-dialog-closed exit)))))
