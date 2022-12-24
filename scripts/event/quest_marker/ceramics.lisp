
(dialog "Still got those ceramic tiles? The locals might be interested...")



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
                       "ceramic tiles"
                       (cargo (player) (car $0) (cdr $0))))
                    c)))
            (if p
                (let ((sale (+ 1000 (* (cdr (assoc 4 qvar)) 2))))
                  ;; Clear out cargo
                  (cargo-set
                   (player)
                   (car (car p))
                   (cdr (car p))
                   "")

                  (dialog
                   (format
                    "<c:Customer:6>Wow, such beautiful tiles! How much would you sell them for? Here, I'll give you %@!"
                    sale))

                  (coins-add sale)

                  (setq on-dialog-closed exit))
              (progn
                (setq on-dialog-closed exit)
                (dialog "Upon closer inspection, no one seems to be home! Good thing too, as it seems that you lost the tiles along the way!")))))))
