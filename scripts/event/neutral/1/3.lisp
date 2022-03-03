;;;
;;; neutral/0/1.lisp
;;;


(dialog "A goblin stronghold approaches... they seem to be willing to negotiate...")



(eval-file "/scripts/event/hostile/2/2.lisp")


(opponent-mode 'neutral)



(let ((temp (+ 500 (choice 500))))
  (setq on-converge
        (lambda
          (dialog
           "<c:goblin king:3>#cackle# You're tresspasssing in my territory! I demand a tribute of "
           (string temp)
           "@! Pay!")

          (dialog-await-y/n)
          (setq on-converge nil)))


  (setq on-dialog-accepted
        (lambda
          (if (> temp (coins))
              (progn
                (opponent-mode 'hostile)
                (dialog "<c:globlin king:3>Thatsss not enough! Letss ssee if theress anything we can take!!"))
            (progn
              (coins-add (- temp))
              (dialog "The goblin king rejoices, having successfully extorted "
                      (string temp)
                      "@.")
              (exit))))))




(setq on-dialog-declined
      (lambda
        (opponent-mode 'hostile)
        (dialog "<g:goblin king:3>YARRRGG!!! PREPARE FOR BOARDING!!!")))


(setq on-hostile-transition
      (lambda
        ;; when the island is preemtively attacked, skip all of the dialog and
        ;; other hooks.
        (eval-file "/scripts/reset_hooks.lisp")))
