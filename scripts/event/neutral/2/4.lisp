;;;
;;; neutral/0/1.lisp
;;;


(dialog "A goblin stronghold approaches... they seem to be willing to negotiate...")



(opponent-init 9 'hostile)


(island-configure
 (opponent)
 '((hull 0 7)
   (mycelium 0 8)
   (mycelium 0 11)
   (energized-hull 0 10)
   (mycelium 0 9)
   (mycelium 0 6 40)
   (mycelium 0 14)
   (mycelium 0 12)
   (mycelium 0 13)
   (mycelium 1 12)
   (mycelium 1 14)
   (mycelium 1 6)
   (mycelium 1 7 40)
   (mycelium 1 11)
   (hull 1 13)
   (hull 1 10)
   (hull 1 8)
   (hull 1 9)
   (power-core 2 9)
   (mycelium 2 5)
   (energized-hull 2 13)
   (energized-hull 2 12)
   (mycelium 2 7)
   (energized-hull 2 14)
   (mycelium 2 6)
   (hull 2 11)
   (hull 2 8)
   (hull 3 8)
   (stairwell 3 11)
   (mycelium 3 7)
   (hull 3 6)
   (mycelium 3 5)
   (power-core 4 10)
   (mycelium 4 7)
   (stacked-hull 4 9)
   (mycelium 4 8)
   (mycelium 4 5)
   (mycelium 4 6)
   (reactor 4 12)
   (stacked-hull 5 9)
   (mycelium 5 8)
   (mycelium 5 7)
   (stairwell 6 11)
   (mycelium 6 6)
   (infirmary 6 9)
   (mycelium 6 7)
   (hull 6 8)
   (missile-silo 7 7)
   (transporter 7 13)
   (forcefield* 7 6)
   (transporter 7 11)
   (mycelium 8 8)
   (mycelium 8 7)
   (mycelium 8 6)
   (transporter 8 13)
   (transporter 8 11)
   (stacked-hull 8 10)
   (mycelium 8 9)))


(flag-show (opponent) 0)


(map
 (lambda (xy)
   (chr-new (opponent) (car xy) (cdr xy) 'hostile 0))
 '((6 . 14)
   (5 . 14)
   (4 . 14)
   (4 . 11)
   (2 . 10)
   (3 . 10)))



(opponent-mode 'neutral)



(let ((val (if (equal (difficulty) 0)
               (+ 900 (choice 500))
             (max (list (+ 900 (choice 500))
                        (/ (coins) 2))))))
  (defn on-converge ()
    (dialog
     "<c:goblin king:3>#cackle# You're tresspasssing in my territory! I demand a tribute of "
     (string val)
     "@! Pay!")

    (dialog-await-binary-q "I'll pay…" "no way!")
    (setq on-converge nil))


  (defn on-dialog-accepted ()
    (if (> val (coins))
        (progn
          (opponent-mode 'hostile)
          (adventure-log-add 32 '())
          (dialog "<c:globlin king:3>Thatsss not enough! Letss ssee if theress anything we can take!!"))
      (progn
        (coins-add (- val))
        (adventure-log-add 31 (list val))
        (dialog "The goblin king rejoices, having successfully extorted "
                (string val)
                "@.")
        (exit)))))



(defn on-dialog-declined ()
  (opponent-mode 'hostile)
  (adventure-log-add 33 '())
  (dialog "<c:goblin king:3>YARRRGG!!! PREPARE FOR BOARDING!!!"))
