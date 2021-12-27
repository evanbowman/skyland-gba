;;;
;;; neutral/1/0.lisp
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


(def after-converge-hook
     (lambda
       (dialog "One of the mercenaries offers to join you crew, for a cost of "
               (string (* 400 (zone)))
               "@. Accept offer?")

       (await-dialog-y/n)
       (def after-converge-hook nil)))


(def after-dialog-accepted-hook
     (lambda

       (def temp (chr-slots (player)))

       (if (> (* 400 (zone)) (coins))
           (dialog "You cannot afford to pay. The mercenaries become frustrated, and cut the transmission.")
         (if temp
             (progn
               (add-coins (- 0 (* 400 (zone))))
               (def temp (get temp (choice (length temp))))
               (add-chr (player) (car temp) (cdr temp) 'neutral 0)
               (rem-chr (opponent) 1 14)
               (def temp (nil))
               (dialog "The mercenary joined your crew!"))
           (dialog "Sadly, there's no room...")))

       (exit-level)))


(def after-dialog-declined-hook
     (lambda
       (dialog "The mercenaries became angry, and cut the transmission.")
       (exit-level)))
