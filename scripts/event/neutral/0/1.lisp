;;;
;;; neutral/0/1.lisp
;;;


(dialog "A heavily armed fortress approaches. Its captain demands to speak with you.")


(opponent-init 5 'neutral)

(island-configure
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



(setq on-converge
      (lambda
        (dialog "The warship requests a tribute of 300@. Will you pay?")

        (dialog-await-y/n)
        (setq on-converge nil)))


(setq on-dialog-accepted
      (lambda
        (if (< (coins) 300)
            (progn
              (dialog "You do not have enough resources to pay! The fortress begins charging its weapons...")
              (opponent-mode 'hostile))
          (progn
            (coins-add -300)
            (dialog "The fortress' captain peers smugly from behind her spectacles. "
                    "She's glad that you understand the nature of the situation.")
            (exit)))))


(setq on-dialog-declined
      (lambda
        (opponent-mode 'hostile)
        (dialog "The fortress begins charging its weapons...")))


(setq on-hostile-transition
      (lambda
        ;; when the island is preemtively attacked, skip all of the dialog and
        ;; other hooks.
        (eval-file "/scripts/reset_hooks.lisp")))
