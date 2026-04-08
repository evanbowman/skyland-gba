;;;
;;; quest_marker/loot.lisp
;;;

(tr-bind-current)

(dialog
 (tr "You arrive at the meeting spot..."))



(opponent-init 6 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (coconut-palm 5 13)))


(setq on-converge
      (lambda ()
        (let ((c (cargo-bays (player))))
          (let ((p (filter (lambda (xy)
                             (equal (tr "loot")
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
                   (format
                    (tr "<c:Contact:2>Sssneaky work! Was starting to think you'd gotten caught! Good haul of machine partsss here - plenty of buyersss waiting! Here'sss %@ for your cut!")
                    temp))

                  (coins-add temp)
                  (adventure-log-add 22 (list temp))

                  (setq on-dialog-closed exit))
              (progn
                (setq on-dialog-closed exit)
                (dialog (tr "Upon closer inspection, no one seems to be home! Good thing too, as it seems that you lost the loot along the way!"))))))))
