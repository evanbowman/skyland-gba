;;;
;;; neutral_0_2.lisp
;;;


(dialog "The remains of an abandoned island emerge from the mist, floating towards you...")



(init-opponent 5 'neutral)


(configure-player
 (opponent)
 '((hull 0 14)
   (power-core 1 13)
   (hull 1 12)
   (workshop 3 13)))


(set 'after-converge-hook
     (lambda
       (set 'temp (+ 400 (cr-choice 900)))

       (set 'after-converge-hook nil)

       (dialog
        "You explore, and discover " (string temp) "$ amongst the ruins!")

       (add-coins temp)

       (exit-level)))
