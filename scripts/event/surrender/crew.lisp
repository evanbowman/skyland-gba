
(dialog
 "<c:goblin pirates:2>We surrender! Honesst, we promise not to pillage any other cassstles!")

(setq on-dialog-closed
      (lambda
        (setq on-dialog-closed '())
        (let ((c (/ (coins-victory) 2)))
          (dialog
           "One of the goblins offers to join your crew, and pay a sum of "
           (string c)
           "@, accept surrender?")

          (dialog-await-y/n)

          (setq on-dialog-accepted
                (lambda
                  (coins-add c)
                  (let ((g (chrs (opponent)))
                        (ss (chr-slots (player))))

                    (unless ss
                      (while (not (construction-sites (player) '(1 . 2)))
                        (terrain (player) (+ (terrain (player)) 1)))

                      (let ((s (construction-sites (player) '(1 . 2))))
                        (room-new (player) (list 'ladder (car (car s)) (cdr (car s))))
                        (setq ss (chr-slots (player)))))

                    (if g
                        (let ((s (get ss (choice (length ss)))))
                          (chr-del (opponent)
                                   (car (car g))
                                   (cdr (car g)))
                          (chr-new (player)
                                   (car s)
                                   (cdr s)
                                   'neutral
                                   '((race . 1)))
                          (adventure-log-add 51 '()))))
                  (exit 2)))

          (setq on-dialog-declined '()))))
