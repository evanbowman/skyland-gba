;;;
;;; neutral/0/1.lisp
;;;


(dialog "A goblin stronghold approaches... they seem to be willing to negotiate...")



(eval-other-file "/scripts/event/hostile/2/2.lisp")


(opponent-mode 'neutral)



(setq after-converge-hook
      (lambda
        (setq temp (+ 500 (choice 500)))
        (dialog
         "The goblins demand a tribute of "
         (string temp)
         "@. Will you pay?")

        (dialog-await-y/n)
        (setq after-converge-hook nil)))


(setq after-dialog-accepted-hook
      (lambda
        (if (> 500 (coins))
            (progn
              (opponent-mode 'hostile)
              (dialog "You cannot afford to pay. Prepare to be boarded!"))
          (progn
            (coins-add (- temp))
            (dialog "The goblin king rejoices, having successfully extorted "
                    (string temp)
                    "@.")
            (exit-level)))))


(setq after-dialog-declined-hook
      (lambda
        (opponent-mode 'hostile)
        (dialog "Prepare to be boarded!")))


(setq hostile-transition-hook
      (lambda
        ;; when the island is preemtively attacked, skip all of the dialog and
        ;; other hooks.
        (eval-other-file "/scripts/reset_hooks.lisp")))
