;;;
;;; quest_marker/ceramics.lisp
;;;


(dialog "Still got those ceramics? The locals might be interested...")



(opponent-init 6 'neutral)

(island-configure
 (opponent)
 '((masonry 0 14 0)
   (coconut-palm 0 12)
   (masonry 1 14 1)
   (masonry 1 13 1)
   (power-core 2 11)
   (masonry 2 14 1)
   (masonry 2 13 0)
   (masonry 3 14 1)
   (masonry 3 13 0)
   (masonry 4 14 1)
   (masonry 4 13 1)
   (coconut-palm 4 11)
   (masonry 5 14 0)))


(setq on-converge
      (lambda ()
        (let ((c (cargo-bays (player))))
          (let ((p (filter
                    (lambda (xy)
                      (equal
                       "ceramic tiles"
                       (cargo (player) (first xy) (second xy))))
                    c)))
            (if p
                (let ((sale (+ 1000 (* (lookup 4 qvar) 2))))
                  ;; Clear out cargo
                  (cargo-set (player)
                             (caar p)
                             (cdr (car p))
                             "")

                  (dialog
                   (format
                    "<c:The Collector:6>Wow, such beautiful craftmanship! I must have them!! How much would you sell them for? Here, take %@!"
                    sale))

                  (coins-add sale)

                  (adventure-log-add 21 (list sale))

                  (setq on-dialog-closed exit))
              (progn
                (setq on-dialog-closed exit)
                (dialog "Upon closer inspection, no one seems to be home! Good thing too, as it seems that you lost the tiles along the way!")))))))
