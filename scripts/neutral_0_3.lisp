;;;
;;; neutral_0_4.lisp
;;;


(dialog "A distress signal sounds over your radio. You change course to investigate...")


(init-opponent 5 'neutral)


(configure-player
 (opponent)
 '((power-core 3 13)
   (hull 0 14)))


(add-chr (opponent) 1 14 'neutral 0)
(add-chr (opponent) 2 14 'neutral 0)


(set 'after-converge-hook
     (lambda
       (dialog "You discover a damaged fortress. Spend 800$ in resources to rescue the survivors?")
       (await-dialog-y/n)
       (set 'after-converge-hook nil)))


(set 'after-dialog-accepted-hook
     (lambda
       (set 'temp (chr-slots (player)))

       (if temp
           (progn
             (add-coins (- 0 800))
             (set 'temp (get temp (cr-choice (length temp))))
             (add-chr (player) (car temp) (cdr temp) 'neutral 0)
             (rem-chr (opponent) 1 14)

             (set 'temp (chr-slots (player)))
             (if temp
                 (progn
                   (set 'temp (get temp (cr-choice (length temp))))
                   (add-chr (player) (car temp) (cdr temp) 'neutral 0)
                   (rem-chr (opponent) 2 14)
                   (dialog "Two survivors joined your crew!"))
               (dialog "You rescue one of the survivors. The other survivor decides "
                       "that your castle is too crowded and declines to come aboard.")))
         (dialog "Sadly, there's no room in your castle for anyone else"))

       (exit-level)))


(set 'after-dialog-declined-hook
     (lambda
       ;; TODO...
       (exit-level)))
