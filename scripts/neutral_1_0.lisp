;;;
;;; neutral_1_0.lisp
;;;


(dialog "You discover a fortress inhabited by some mercenaries...")


(init-opponent 8 'neutral)


(configure-player
 (opponent)
 '((power-core 3 13)
   (missile-silo 5 13)
   (missile-silo 6 13)
   (missile-silo 7 13)))


(add-chr (opponent) 1 14 'neutral 0)
(add-chr (opponent) 2 14 'neutral 0)


(set 'after-converge-hook
     (lambda
       (dialog "One of the mercenaries offers to join you crew, for a cost of 400$. Accept offer?")

       (await-dialog-y/n)
       (set 'after-converge-hook nil)))


(set 'after-dialog-accepted-hook
     (lambda

       (set 'temp (chr-slots (player)))

       (if temp
           (progn
             (add-coins (- 0 400))
             (set 'temp (get temp (cr-choice (length temp))))
             (add-chr (player) (car temp) (cdr temp) 'neutral 0)
             (rem-chr (opponent) 1 14)
             (set 'temp (nil))
             (dialog "The mercenary joined your crew!"))
         (dialog "Sadly, there's no room..."))

       (exit-level)))


(set 'after-dialog-declined-hook
     (lambda
       (dialog "The mercenaries became angry, and cut the transmission.")
       (exit-level)))
