;;;
;;; neutral/1/4.lisp
;;;


(dialog
 "<b:/scripts/misc/img/overgrown_isle.img.bin>"
 "A nearby island seems to be transmitting an unusual distress signal...")


(opponent-init 7 'neutral)

(island-configure
 (opponent)
 '((masonry 0 14)
   (masonry 0 13)
   (mycelium 0 12)
   (mycelium 0 11)
   (power-core 1 13)
   (masonry 1 12)
   (mycelium 1 11)
   (masonry 2 12)
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
   (masonry 6 14)
   (mycelium 6 12)
   (mycelium 6 13)
   (mycelium 6 11)
   (mycelium 6 10)
   (mycelium 6 9)))

(flag-show (opponent) 4)


(defn on-converge [0]
  (dialog "Upon closer inspection, you find that the castle may contain valuable cargo, but it's overgrown with mycelium. You can explore, although there's some risk of cross-contamination. Board anyway?")
  (dialog-await-y/n))


(defn on-dialog-accepted [0]
  (let ((end (lambda
               ((eval-file "/scripts/util/pickup_cart.lisp") 6
                "One of your crewmembers finds a data cartridge tangled in the fungal roots..."))))
    (if (choice 3)
        (progn
          (let ((locs (construction-sites (player) '(1 . 1))))
            (when locs
              (let ((c (get locs (choice (length locs)))))
                (room-new (player) (list 'mycelium (car c) (cdr c))))
              (adventure-log-add 34 '())
              (dialog "While attempting to board, several spores on the castle burst, infesting your island with mycelium!")))
          (end))
      (let ((temp (+ 1000 (choice 1000))))
        (dialog "You explore, and find cargo worth " (string temp) "@!")
        (coins-add temp)
        (adventure-log-add 35 (list temp))
        (end)))))



(setq on-dialog-declined exit)
