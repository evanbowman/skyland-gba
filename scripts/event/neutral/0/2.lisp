;;;
;;; neutral/0/2.lisp
;;;


(dialog "The remains of an abandoned island emerge from the mist, floating towards you...")



(opponent-init 5 'neutral)


(island-configure
 (opponent)
 '((hull 0 14)
   (power-core 1 13)
   (hull 1 12)
   (workshop 3 13)))


(if (choice 2)
    (secret
     1 12
     "To the earth below, I will not go"))



(setq on-converge
      (lambda
        (setq temp (+ 400 (choice 900)))

        (setq on-converge nil)

        (dialog
         "You explore, and discover " (string temp) "@ amongst the ruins!")

        (coins-add temp)

        ((eval-file "/scripts/util/pickup_cart.lisp") 1
         "Just as you're turning to leave, you spot a data cartridge sitting on an unfinished game of checkers.")))
