;;;
;;; neutral/0/1.lisp
;;;


(dialog "A goblin stronghold approaches... they seem to be willing to negotiate...")



(eval-file "/scripts/event/hostile/2/2.lisp")


(opponent-mode 'neutral)



(let ((val (if (equal (diff) 0)
               (+ 900 (choice 500))
             (max (list (+ 500 (choice 500))
                        (/ (coins) 3))))))
  (setq on-converge
        (lambda
          (dialog
           "<c:goblin king:3>#cackle# You're tresspasssing in my territory! I demand a tribute of "
           (string val)
           "@! Pay!")

          (dialog-await-y/n)
          (setq on-converge nil)))


  (setq on-dialog-accepted
        (lambda
          (if (> val (coins))
              (progn
                (opponent-mode 'hostile)
                (adventure-log-add 32 '())
                (dialog "<c:globlin king:3>Thatsss not enough! Letss ssee if theress anything we can take!!"))
            (progn
              (coins-add (- val))
              (dialog "The goblin king rejoices, having successfully extorted "
                      (string val)
                      "@.")
              (adventure-log-add 31 (list val))
              (exit))))))




(setq on-dialog-declined
      (lambda
        (opponent-mode 'hostile)
        (adventure-log-add 33 '())
        (dialog "<c:goblin king:3>YARRRGG!!! PREPARE FOR BOARDING!!!")))
