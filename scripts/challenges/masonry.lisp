;;;
;;; masonry.lisp
;;;



(setq on-fadein
      (lambda ()
        (dialog
         "<c:goblin king:3>Masssonry! Massonry! I hate it! Sssooo old fasshioned! "
         "Destroy all of it for me? Don't desstroy anything else!")))



(defn challenge-hint ()
  (dialog "Try upgrading your workshop to a manufactory, regular weapons will not be enough!"))



(setq on-room-destroyed
      (lambda (isle sym)
        (if (equal isle (opponent))
            (if (not (equal sym 'masonry))
                (progn
                  (dialog "<c:goblin king:3>Gaahh, I ssaid only masssonry!")
                  (setq on-room-destroyed nil)
                  (setq on-dialog-closed
                        (lambda ()
                          (exit 3))))
              ;; NOTE: equal 1 because the room is in the process of being
              ;; destroyed, it still exists on the island.
              (if (equal 1 (room-count (opponent) 'masonry))
                  (progn
                    (dialog "<c:goblin king:3>Wowowow! Beautiful! Ssspectacular!")
                    (challenge-complete 3)
                    (setq on-dialog-closed
                          (lambda ()
                            (exit 2)))))))))



(terrain-set (player) 8)
(island-configure
 (player)
 '((power-core 1 13)
   (workshop 3 13)))

(flag-show (player) 0)


(coins-add 9000)


(opponent-init 13 'hostile)


(island-configure
 (opponent)
 '((hull 0 12)
   (hull 0 9)
   (hull 0 14)
   (hull 0 10)
   (hull 0 13)
   (hull 0 6)
   (hull 1 9)
   (hull 1 7)
   (hull 2 8)
   (hull 2 9)
   (hull 3 9)
   (hull 4 9)
   (hull 5 9)
   (hull 6 9)
   (hull 7 10)
   (hull 8 10)
   (hull 9 8)
   (hull 9 10)
   (hull 9 11)
   (hull 9 9)
   (hull 11 12)
   (hull 11 8)
   (hull 12 12)
   (masonry 0 7)
   (masonry 0 8)
   (masonry 0 11)
   (masonry 1 11)
   (masonry 1 8)
   (dynamite 1 10)
   (dynamite 1 14)
   (dynamite 1 12)
   (dynamite 1 13)
   (masonry 2 7)
   (masonry 2 11)
   (dynamite 2 12)
   (workshop 2 13)
   (dynamite 2 10)
   (masonry 3 7)
   (masonry 3 8)
   (masonry 3 11)
   (dynamite 3 10)
   (dynamite 3 12)
   (dynamite 4 13)
   (masonry 4 7)
   (dynamite 4 12)
   (masonry 4 8)
   (dynamite 4 14)
   (masonry 4 11)
   (dynamite 4 10)
   (masonry 5 7)
   (masonry 5 12)
   (masonry 5 14)
   (masonry 5 8)
   (masonry 5 11)
   (masonry 5 13)
   (dynamite 5 10)
   (masonry 6 7)
   (dynamite 6 13)
   (masonry 6 14)
   (dynamite 6 12)
   (masonry 6 8)
   (dynamite 6 10)
   (dynamite 6 11)
   (masonry 7 8)
   (masonry 7 13)
   (masonry 7 12)
   (masonry 7 14)
   (dynamite 7 11)
   (masonry 7 9)
   (stairwell 8 11)
   (masonry 8 9)
   (dynamite 9 13)
   (dynamite 9 14)
   (dynamite 9 12)
   (masonry 10 11)
   (masonry 10 9)
   (masonry 10 8)
   (masonry 10 12)
   (masonry 10 14)
   (masonry 10 10)
   (masonry 10 13)
   (dynamite 11 9)
   (power-core 11 13)
   (masonry 11 10)
   (masonry 11 11)))
