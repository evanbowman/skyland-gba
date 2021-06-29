;;;
;;; neutral_0_1.lisp
;;;


(dialog "A heavily armed fortress approaches. Its captain demands to speak with you.")


(init-opponent 5 'neutral)

(configure-player
 (opponent)
 '((power-core 3 13)
   (cannon 0 14)
   (cannon 0 13)
   (cannon 0 12)
   (missile-silo 3 11)
   (hull 4 12)
   (hull 1 14)
   (hull 2 14)
   (hull 1 13)
   (hull 2 13)
   (hull 1 12)
   (hull 2 12)))



(set 'after-converge-hook
     (lambda
       (dialog "The warship requests a tribute of 300$. Will you pay?")

       (await-dialog-y/n)
       (set 'after-converge-hook nil)))


(set 'after-dialog-accepted-hook
     (lambda
       (add-coins (- 0 300))
       (dialog "The fortress' captain peers smugly from behind her spectacles. "
               "She's glad that you understand the nature of the situation.")
       (exit-level)))


(set 'after-dialog-declined-hook
     (lambda
       (swap-opponent 'hostile)
       (dialog "The fortress begins charging its weapons...")))


(set 'hostile-transition-hook
     (lambda
       ;; when the island is preemtively attacked, skip all of the dialog and
       ;; other hooks.
       (eval-other-file "reset_hooks.lisp")))
