;;;
;;; neutral/2/2_alt0.lisp
;;;

(tr-bind-current)


(dialog (tr "An ancient warship approaches, flying a pirate flag. <B:0> The fortress gives off a strange radiation signature, but the pirates have not yet decided to attack..."))



(eval-file "/scripts/event/hostile/3/0.lisp")


(opponent-mode 'neutral)

(flag-show (opponent) flag-id-pirate)


(let ((val (+ 1000 (choice 800))))
  (defn on-converge ()
    (setq on-converge nil)
    (let ((msg (string (tr "The pirates seem to have stolen a powerful imperial assault vessel. ")
                       (format (tr "They demand %@ and make crude gestures. Will you pay?") val))))
      (if (dialog-await-binary-q-w/lore msg
                                        (format (tr "Pay %@.") val)
                                        (tr "No way!")
                                        (tr '(("What's an assault ship?" .
                                               "Imperial assault ships were involved in the surface wars. Not many still exist, and not much is known about them. <B:0> The pirates are getting impatient. Pay the bribe?"))))
          (on-dialog-accepted)
          (on-dialog-declined))))


  (setq on-dialog-accepted
        (lambda ()
          (if (> 500 (coins))
              (progn
                (opponent-mode 'hostile)
                (adventure-log-add 42 '())
                (dialog (tr "You cannot afford to pay. Prepare for heavy damage...")))
            (progn
              (coins-add (- val))
              (dialog (tr "The pirates accept your bribe and move on."))
              (adventure-log-add 43 '())
              (exit))))))


(setq on-dialog-declined
      (lambda ()
        (opponent-mode 'hostile)
        (adventure-log-add 42 '())
        (dialog (tr "Prepare for attack!"))))
