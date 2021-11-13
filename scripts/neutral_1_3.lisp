;;;
;;; neutral_0_1.lisp
;;;


(dialog "A goblin stronghold approaches... they seem to be willing to negotiate...")



(eval-other-file "hostile_2_2.lisp")


(swap-opponent 'neutral)



(set 'after-converge-hook
     (lambda
       (set 'temp (+ 500 (choice 500)))
       (dialog
        "The goblins demand a tribute of "
        (string temp)
        "@. Will you pay?")

       (await-dialog-y/n)
       (set 'after-converge-hook nil)))


(set 'after-dialog-accepted-hook
     (lambda
       (if (> 500 (coins))
           (progn
             (swap-opponent 'hostile)
             (dialog "You cannot afford to pay. Prepare to be boarded!"))
         (progn
           (add-coins (- 0 temp))
           (dialog "The goblin king rejoices, having successfully extorted "
                   (string temp)
                   "@.")
           (exit-level)))))


(set 'after-dialog-declined-hook
     (lambda
       (swap-opponent 'hostile)
       (dialog "Prepare to be boarded!")))


(set 'hostile-transition-hook
     (lambda
       ;; when the island is preemtively attacked, skip all of the dialog and
       ;; other hooks.
       (eval-other-file "reset_hooks.lisp")))
