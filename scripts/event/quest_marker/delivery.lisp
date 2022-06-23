
(dialog "You arrive at the delivery address...")



(opponent-init 6 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (coconut-palm 5 13)))


(setq on-converge
      (lambda
        (let ((c (cargo-bays (player))))
          (let ((p (filter
                    (lambda
                      (equal
                       "parcel"
                       (cargo (player) (car $0) (cdr $0))))
                    c)))
            (if p
                (let ((temp (+ 1500 (choice 2000))))
                  ;; Clear out cargo
                  (cargo-set
                   (player)
                   (car (car p))
                   (cdr (car p))
                   "")

                  (dialog
                   "<c:Customer:6>Thank you! I wasn't sure it would arrive! Here's "
                   (string temp)
                   "@ for your trouble!")

                  (coins-add temp)

                  (setq on-dialog-closed exit))
              (progn
                (setq on-dialog-closed exit)
                (dialog "Upon closer inspection, no one seems to be home! Good thing too, as it seems that you lost the parcel along the way!")))))))
