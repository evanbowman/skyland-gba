;;;
;;; neutral/0/1.lisp
;;;


(dialog "A goblin stronghold approaches... they seem to be willing to negotiate...")



(eval-file "/scripts/event/hostile/2/2.lisp")


(opponent-mode 'neutral)



(setq on-converge
      (lambda
        (setq temp (+ 500 (choice 500)))
        (dialog
         "*cackle* You're tresspasssing in my territory! I demand a tribute of "
         (string temp)
         "@! Pay!")

        (dialog-decor "goblin king" 3)
        (dialog-await-y/n)
        (setq on-converge nil)))


(setq on-dialog-accepted
      (lambda
        (if (> 500 (coins))
            (progn
              (opponent-mode 'hostile)
              (dialog "Thatsss not enough! Letss ssee if theress anything we can take!!")
              (dialog-decor "goblin king" 3))
          (progn
            (coins-add (- temp))
            (dialog "The goblin king rejoices, having successfully extorted "
                    (string temp)
                    "@.")
            (exit)))))


(setq on-dialog-declined
      (lambda
        (opponent-mode 'hostile)
        (dialog "YARRRGG!!! PREPARE FOR BOARDING!!!")
        (dialog-decor "goblin king" 3)))


(setq on-hostile-transition
      (lambda
        ;; when the island is preemtively attacked, skip all of the dialog and
        ;; other hooks.
        (eval-file "/scripts/reset_hooks.lisp")))
