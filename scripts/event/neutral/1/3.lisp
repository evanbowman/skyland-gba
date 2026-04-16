;;;
;;; neutral/1/3.lisp
;;;

(tr-bind-current)


(dialog (tr "A goblin stronghold approaches... they seem to be willing to negotiate..."))



(eval-file "/scripts/event/hostile/2/2.lisp")


(opponent-mode 'neutral)



(let ((fee (if (equal (difficulty) difficulty-beginner)
               (+ 900 (choice 500))
             (max (list (+ 500 (choice 500))
                        (floor (/ (coins) 3)))))))

  (defn on-converge ()
    (setq on-converge nil)
    (if (dialog-await-binary-q (format (tr "<c:Goblin King:3>#cackle# You're tressspasssing in my territory! I demand a tribute of %@! Pay!") fee)
                               (tr "I'll pay…")
                               (tr "No way!"))
        (on-dialog-accepted)
        (on-dialog-declined)))


  (defn on-dialog-accepted ()
    (if (> fee (coins))
        (progn
          (opponent-mode 'hostile)
          (adventure-log-add 32 '())
          (dialog (tr "<c:Goblin King:3>Thatsss not enough! Letsss sssee if theresss anything we can take!!")))
        (progn
          (coins-add (- fee))
          (await (dialog* (format (tr "The Goblin King rejoices, having successfully extorted %@.")
                                  fee)))
          (adventure-log-add 31 (list fee))
          (exit)))))




(setq on-dialog-declined
      (lambda ()
        (opponent-mode 'hostile)
        (adventure-log-add 33 '())
        (dialog (tr "<c:Goblin King:3>YARRRGG!!! <a:SHAKE>PREPARE <a:SHAKE>FOR <a:SHAKE>BOARDING!!!"))))
