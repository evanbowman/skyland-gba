;;;
;;; neutral/0/0.lisp
;;;


(dialog "In the distance, you see an island inhabited by a lone castaway...")


(init-opponent 5 'neutral)


(configure-player
 (opponent)
 '((power-core 3 13)))


(add-chr (opponent) 1 14 'neutral 0)


(setq after-converge-hook
      (lambda
        (dialog "Invite castaway aboard?")

        (await-dialog-y/n)
        (setq after-converge-hook nil)))


(setq after-dialog-accepted-hook
      (lambda

        (setq temp (chr-slots (player)))

        (if temp
            (progn
              (setq temp (get temp (choice (length temp))))
              (add-chr (player) (car temp) (cdr temp) 'neutral 0)
              (rem-chr (opponent) 1 14)
              (setq temp '())
              (dialog "The castaway joined your crew!"))
          (dialog "Sadly, there's no room..."))

        (exit-level)))


(setq after-dialog-declined-hook
      (lambda
        ;; TODO...
        (exit-level)))
