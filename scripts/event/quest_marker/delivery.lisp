;;;
;;; quest_marker/delivery.lisp
;;;


(dialog
 "<b:/scripts/data/img/castaway.img.bin>"
 "You arrive at the delivery address...")



(opponent-init 6 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (coconut-palm 5 13)))


(setq on-converge
      (lambda ()
        (let ((c (cargo-bays (player))))
          (let ((p (filter (lambda (xy)
                             (equal "parcel"
                                    (cargo (player) (first xy) (second xy))))
                           c)))
            (if p
                (let ((temp (+ 2500 (choice 2000))))
                  ;; Clear out cargo
                  (cargo-set
                   (player)
                   (caar p)
                   (cdr (car p))
                   "")

                  (dialog
                   "<c:Customer:16>Thank you! I wasn't sure it would arrive! Now I can use these machine parts to fix my engines! Here's "
                   (string temp)
                   "@ for your trouble!")

                  (coins-add temp)
                  (adventure-log-add 22 (list temp))

                  (setq on-dialog-closed exit))
              (progn
                (setq on-dialog-closed exit)
                (dialog "Upon closer inspection, no one seems to be home! Good thing too, as it seems that you lost the parcel along the way!")))))))
