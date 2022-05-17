;;;
;;; neutral/0/3.lisp
;;;


(dialog "Some merchants broadcast an advertisement for advanced technology! Let's see if they have anything useful!")


(island-configure
 (player)
 '((power-core 3 13)
   (hull 0 14)))
(coins-set 1300)


(opponent-init 5 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 0 14)))

(chr-new (opponent) 1 14 'neutral 0)
(chr-new (opponent) 2 14 'neutral 0)


(let ((temp (sample '((arc-gun . (1 . 1))
                      (fire-charge . (2 . 1))))))
  (setq on-converge
        (lambda
          (dialog "<c:merchant:7> We ordered too many "
                  (string (car temp))
                  "s and we're having a big sale today! Much cheaper than if you built them yourself. 1300@ for two, what do you say?")
          (dialog-await-y/n)
          (setq on-converge nil)))



  (setq on-dialog-accepted
        (lambda
          (if (< (coins) 1300)
              (progn
                (dialog "<c:merchant:7> Sorry, that's not enough money!")
                (exit))
            (progn
              (coins-add -1300)
              (sel-input
               (cdr temp)
               (string "place first " (car temp) ":")
               (lambda
                 (room-new (player) (list (car temp) $1 $2))
                 (sel-input
                  (cdr temp)
                  (string "place second " (car temp) ":")
                  (lambda
                    (room-new (player) (list (car temp) $1 $2))
                    (dialog "<c:merchant:7> Looks great! You made a fine choice!")
                    (setq on-dialog-closed exit))))))))))

(gc) ;; just in case, no harm in running it.



(setq on-dialog-declined
      (lambda
        (exit)))
