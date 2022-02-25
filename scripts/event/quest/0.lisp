;;;
;;; TODO...
;;;


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
                (dialog "Wonderful! I'll mark the address with an x on your sky chart!")
                (setq on-dialog-closed
                      (lambda
                        ))))

        (setq on-dialog-declined exit)))
