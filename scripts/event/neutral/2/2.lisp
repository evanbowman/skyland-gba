;;;
;;; neutral/2/2.lisp
;;;



(dialog "An ancient warship approaches, flying a pirate flag. The fortress gives off a strange radiation signature, but the pirates have not yet decided to attack...")



(eval-other-file "/scripts/event/hostile/3/0.lisp")


(swap-opponent 'neutral)



(setq after-converge-hook
      (lambda
        (setq temp (+ 1000 (choice 800)))
        (dialog
         "The pirates seem to have stolen a powerful imperial assault vessel. They demand "
         (string temp)
         "@ and make crude gestures. Will you pay?")

        (await-dialog-y/n)
        (setq after-converge-hook nil)))


(setq after-dialog-accepted-hook
      (lambda
        (if (> 500 (coins))
            (progn
              (swap-opponent 'hostile)
              (dialog "You cannot afford to pay. Prepare for heavy damage..."))
          (progn
            (add-coins (- 0 temp))
            (dialog "The pirates accept your bribe and move on.")
            (exit-level)))))


(setq after-dialog-declined-hook
      (lambda
        (swap-opponent 'hostile)
        (dialog "Prepare for attack!")))


(setq hostile-transition-hook
      (lambda
        ;; when the island is preemtively attacked, skip all of the dialog and
        ;; other hooks.
        (eval-other-file "/scripts/reset_hooks.lisp")))
