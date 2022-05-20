;;;
;;; neutral/1/4.lisp
;;;


(dialog "A nearby island seems to be transmitting an unusual distress signal...")


(opponent-init 7 'neutral)

(island-configure
 (opponent)
 '((hull 0 14)
   (hull 0 13)
   (mycelium 0 12)
   (mycelium 0 11)
   (power-core 1 13)
   (hull 1 12)
   (mycelium 1 11)
   (hull 2 12)
   (mycelium 2 11)
   (mycelium 3 12)
   (mycelium 3 7)
   (mycelium 3 11)
   (mycelium 3 10)
   (mycelium 3 9)
   (manufactory 3 13)
   (mycelium 3 8)
   (mycelium 4 7)
   (radar 4 8)
   (power-core 4 10)
   (mycelium 4 12)
   (mycelium 5 7)
   (mycelium 5 8)
   (mycelium 5 9)
   (mycelium 5 12)
   (hull 6 14)
   (mycelium 6 12)
   (mycelium 6 13)
   (mycelium 6 11)
   (mycelium 6 10)
   (mycelium 6 9)))


(defn on-converge
  (dialog "Upon closer inspection, you find that the castle may contain valuable cargo, but it's overgrown with mycelium. You can explore, although there's some risk of cross-contamination. Board anyway?")
  (dialog-await-y/n))


(defn on-dialog-accepted
  (if (choice 3)
      (progn
        (let ((locs (construction-sites (player) '(1 . 1))))
          (if locs
              (progn
                (let ((c (get locs (choice (length locs)))))
                  (room-new (player) (list 'mycelium (car c) (cdr c))))
                (dialog "While attempting to board, several spores on the castle burst, infesting your island with mycelium!"))))
        (exit))
    (progn
      (let ((temp (+ 1000 (choice 1000))))
        (dialog "You explore, and find cargo worth " (string temp) "@!")
        (coins-add temp)
        (exit)))))


(setq on-dialog-declined exit)
