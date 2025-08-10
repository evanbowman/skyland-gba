;;;
;;; storm_king.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(dialog
 "The storm overtakes your castle... A massive fortress emerges from the tempest... "
 (cond
  ((equal (zone) 0) "There is little hope of survival...")
  ((equal (zone) 1) "You probably aren't strong enough yet...")
  ((equal (zone) 2) "You just might survive...")
  ((equal (zone) 3) "Ready for the final showdown?")))


(setq on-fadein
      (lambda ()
        (dialog "<c:Storm King:4> . . .")))


(adventure-log-add 48 '())


(opponent-init 13 'hostile)


(island-configure
 (opponent)
 '((forcefield 0 10)
   (forcefield 0 12)
   (forcefield 0 9)
   (forcefield 0 13)
   (forcefield 0 8)
   (forcefield 0 11)
   (forcefield 0 14)
   (arc-gun 1 7)
   (flak-gun 1 10)
   (flak-gun 1 8)
   (forcefield 1 14)
   (arc-gun 1 12)
   (flak-gun 1 9)
   (forcefield 1 13)
   (flak-gun 1 11)
   (stacked-hull 1 6)
   (energized-hull 2 7)
   (decimator 2 13)
   (energized-hull 2 12)
   (stairwell 3 8)
   (hull 3 12)
   (energized-hull 3 7)
   (power-core 4 8)
   (mirror-hull 4 13)
   (mirror-hull 4 14)
   (hull 4 12)
   (transporter 4 10)
   (hull 4 7)
   (ion-fizzler 5 10)
   (ion-fizzler 5 13)
   (hull 5 12)
   (hull 5 7)
   (hull 6 7)
   (transporter 6 8)
   (hull 6 10)
   (stairwell 6 11)
   (forcefield* 7 5)
   (transporter 7 8)
   (infirmary 7 13)
   (missile-silo 7 6)
   (reactor 7 10)
   (forcefield* 8 6)
   (missile-silo 8 7)
   (hull 8 9)
   (missile-silo 9 8)
   (forcefield 9 7)
   (hull 9 10)
   (reactor 9 12)
   (energized-hull 9 11)
   (energized-hull 10 11)
   (hull 10 10)
   (missile-silo 10 8)
   (forcefield 10 7)
   (hull 11 7)
   (reactor 11 12)
   (energized-hull 11 11)
   (hull 11 10)
   (power-core 11 8)
   (energized-hull 12 11)
   (hull 12 10)
   (hull 12 7)))


(when (> (difficulty) 1)
  (eval-file "/scripts/event/storm_king_hard.lisp"))


(if (> (difficulty) 1)
    (let ((s (filter (lambda (xy)
                       (> (second xy) 10))
                     (chr-slots (opponent)))))
      (map (lambda (x)
             (let ((sl (get s x)))
               (chr-new (opponent) (car sl) (cdr sl) 'hostile 0)))
           (range 2 9))
      (chr-new (opponent) 4 14 'hostile 0))
  (progn
    (chr-new (opponent) 3 14 'hostile 0)
    (chr-new (opponent) 7 12 'hostile 0)
    (chr-new (opponent) 8 12 'hostile 0)
    (chr-new (opponent) 6 14 'hostile 0)
    (chr-new (opponent) 7 14 'hostile 0)))
