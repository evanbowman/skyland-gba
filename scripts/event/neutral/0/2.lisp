;;;
;;; neutral/0/2.lisp
;;;

(tr-bind-current)

(dialog (tr "The remains of an abandoned island emerge from the mist, floating towards you..."))


(let ((file "/scripts/event/neutral/0/2_layouts.txt"))
  (let ((opt (eval (read (file-get-line file (+ 1 (choice (file-line-count file))))))))
    (opponent-init (car opt) 'neutral)
    (island-configure (opponent) (cdr opt))))


(if (choice 2)
    (secret
     1 12
     (tr "To the earth below, I will not go.")))



(defn on-converge ()
  (setq on-converge nil)

  (let ((amt (+ 400 (choice 900))))

    (when (equal (zone) 3)
      (setq amt (+ 800 (choice 900))))

    (adventure-log-add 11 (list amt))
    (coins-add amt)

    (await (dialog*
            (format (tr "You explore, and discover %@ amongst the ruins!") amt)))

    (pickup-cart 1 (tr "Just as you're turning to leave, you spot a data cartridge sitting on an unfinished game of checkers."))
    (exit)))
