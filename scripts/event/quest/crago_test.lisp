;;;
;;; quest.lisp
;;;



(dialog
 "An island on the horizon broadcasts a message... a trader wants a smuggler to help transport some cargo, make sure that you build a cargo-bay!")


(opponent-init 5 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)))


(chr-add (opponent) 1 14 'neutral 0)



(setq after-converge-hook
      (lambda
        (dialog "Would you like to carry this cargo for me?")
        (dialog-await-y/n)

        (setq after-dialog-accepted-hook
              (lambda
                ((eval-file "/scripts/utils/build_cargo_bay.lisp")
                 (lambda
                   (dialog (string $0))))))

        (setq after-dialog-declined-hook exit-level)))
