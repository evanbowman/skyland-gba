;;;
;;; neutral/2/2.lisp
;;;



(dialog "An ancient warship approaches, flying a pirate flag. The fortress gives off a strange radiation signature, but the pirates have not yet decided to attack...")



(eval-file "/scripts/event/hostile/3/0.lisp")


(opponent-mode 'neutral)

(flag-show (opponent) 2)


(let ((val (+ 1000 (choice 800))))
  (setq on-converge
        (lambda
          (dialog
           "The pirates seem to have stolen a powerful imperial assault vessel. They demand "
           (string val)
           "@ and make crude gestures. Will you pay?")

          (dialog-await-y/n)
          (setq on-converge nil)))


  (setq on-dialog-accepted
        (lambda
          (if (> 500 (scrap))
              (progn
                (opponent-mode 'hostile)
                (adventure-log-add 42 '())
                (dialog "You cannot afford to pay. Prepare for heavy damage..."))
            (progn
              (scrap-add (- val))
              (dialog "The pirates accept your bribe and move on.")
              (adventure-log-add 43 '())
              (exit))))))


(setq on-dialog-declined
      (lambda
        (opponent-mode 'hostile)
        (adventure-log-add 42 '())
        (dialog "Prepare for attack!")))
