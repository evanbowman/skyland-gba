;;;
;;; quest_marker/nanas.lisp
;;;


(dialog "Could it be... the stolen bananas!")


(opponent-init 11 'hostile)


(island-configure
 (opponent)
 '((hull 0 12)
   (hull 0 14)
   (hull 0 13)
   (hull 1 11)
   (hull 1 12)
   (power-core 1 13)
   (missile-silo 2 11)
   (banana-plant 3 14)
   (banana-plant 4 14)
   (banana-plant 5 14)
   (banana-plant 6 14)
   (power-core 7 13)
   (hull 7 12)
   (banana-plant 7 11)
   (hull 8 12)
   (banana-plant 8 11)
   (missile-silo 9 13)
   (missile-silo 10 13)))


(setq on-converge
      (lambda ()
        (dialog
         "<c:Banana Man:8>MY BANANAS!!! HAND THEM OVER!!!")

        (setq on-converge nil)

        (setq on-dialog-closed
              (lambda ()
                (dialog
                 "<c:Goblins:2>Argh, he's sstill chasing us! This guy is relentlessss! Alright, it's not worth the trouble, we'll just give you the bananas back.")
                (setq on-dialog-closed
                      (lambda ()
                        (island-configure
                         (opponent)
                         '((hull 0 12)
                           (hull 0 14)
                           (hull 0 13)
                           (hull 1 11)
                           (hull 1 12)
                           (power-core 1 13)
                           (missile-silo 2 11)
                           (power-core 7 13)
                           (hull 7 12)
                           (banana-plant 7 11)
                           (hull 8 12)
                           (banana-plant 8 11)
                           (missile-silo 9 13)
                           (missile-silo 10 13)))

                        (map (lambda (xy)
                               (room-new (player)
                                         (list 'banana-plant (first xy) (second xy))))
                             (construction-sites (player) '(1 . 1)))

                        (adventure-log-add 24 '())

                        (dialog
                         "<c:Banana Man:8>Hooray! My bananas! Time for Banana Breakfast!")
                        (setq on-dialog-closed
                              (lambda ()
                                (achieve 11)
                                (coins-add 2000)
                                (exit 2)))))))))
