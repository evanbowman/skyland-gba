

(dialog
 "<b:/scripts/data/img/market_quest.img.bin>"
 "You arrive at a busy trading port. "
 "Some merchants broadcast a message asking for assistance...")


(opponent-init 9 'neutral)

(island-configure
 (opponent)
 '((bronze-hull 0 11)
   (bronze-hull 0 10)
   (hull 0 9)
   (statue 0 7)
   (bell 2 9)
   (bronze-hull 0 14)
   (bronze-hull 0 12)
   (bronze-hull 0 13)
   (workshop 1 11)
   (bridge 1 10)
   (masonry 1 14 3)
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
   (windmill 5 14)
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

(flag-show (opponent) 6)


(setq on-converge
      (lambda ()
        (dialog
         "<c:merchant:7>We promised to deliver some cargo to our customers, but with "
         "this storm approaching, we don't think we can make the delivery. "
         "Can you help? We'll pay you a bit upfront, and I'm sure that they'll tip "
         "you generously.")
        (dialog-await-binary-q-w/lore "I accept!" "sorry, but no."
                                      '(("explain deliveries?" .
                                         "<c:merchant:7>Usually we have stuff delivered by balloon. Customers place orders, and we send then out by airship. But with this terrible weather coming in, it's too risky to send anything. Can you help us out?")))

        (setq on-dialog-accepted
              (lambda ()
                (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
                  (if m
                      (run-util-script
                       "find-or-create-cargo-bay"
                       (lambda (x y)
                         (push 'quests (cons "delivery.lisp" m))
                         (coins-add 500)
                         (push 'qids 0)
                         (adventure-log-add 16 '())
                         (cargo-set (player) x y "parcel")
                         (dialog "<c:merchant:7>Wonderful! I'll mark the address "
                                 "with an * on your sky chart!")
                         (run-util-script "pickup-cart" 5
                                          "Amazed by the picturesque view from the market center, one of your crew members took a photo, and recorded it on a data cartridge..."
                                          exit)))
                      (progn
                        (dialog
                         "<c:merchant:7>Oh, I'm so sorry! I just got a call from the customer, "
                         "she had to relocate to flee the storm. Here's 400@ for your trouble.")
                        (setq on-dialog-closed
                              (lambda ()
                                (coins-add 400)
                                (exit))))))))

        (setq on-dialog-declined
              (lambda ()
                (dialog "<c:merchant:7>I understand... I guess we'll try to find someone else...")
                (setq on-dialog-closed exit)))))
