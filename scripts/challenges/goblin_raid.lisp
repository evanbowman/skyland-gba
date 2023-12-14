;;;
;;; goblin_raid.lisp
;;;


(coins-add 8500)


(defn on-victory [0]
  (challenge-complete 1))



(defn challenge-hint [0]
  (dialog "Sorry, no hints for this one."))


(setq on-fadein
      (lambda
        (dialog "<c:goblin:2>Hey King, we've found a defensselesss cassstle!")

        (setq on-dialog-closed
              (lambda
                (dialog "<c:goblin king:3>#cackle# minionsss, pepare for boarding!")
                (setq on-dialog-closed nil)))))




(terrain-set (player) 8)
(island-configure
 (player)
 '((stairwell 0 11)
   (power-core 1 13)
   (workshop 3 13)
   (workshop 1 11)
   (radar 5 13)))

(flag-show (player) 0)

(chr-new (player) 2 14 'neutral nil)



(opponent-init 11 'hostile)



(island-configure
 (opponent)
 '((power-core 3 13)
   (stairwell 5 11)
   (power-core 8 13)
   (hull 7 14)
   (hull 7 13)
   (hull 6 13)
   (hull 6 14)
   (stairwell 10 11)
   (hull 10 9)
   (hull 10 10)
   (hull 9 9)
   (hull 8 9)
   (hull 8 8)
   (hull 9 8)
   (hull 3 8)
   (hull 4 8)
   (transporter 3 11)
   (transporter 4 11)
   (transporter 6 11)
   (transporter 7 11)
   (infirmary 8 11)
   (hull 2 14)
   (hull 2 13)
   (hull 2 12)
   (hull 2 11)
   (hull 1 14)
   (hull 1 13)
   (hull 1 12)
   (hull 1 11)
   (hull 0 14)
   (hull 0 13)
   (hull 0 12)
   (hull 0 11)

   (hull 2 10)

   (hull 3 10)
   (hull 4 10)
   (hull 5 10)
   (hull 6 10)
   (hull 7 10)
   (hull 8 10)
   (hull 9 10)

   (hull 3 9)
   (hull 4 9)
   (hull 5 9)
   (hull 6 9)
   (hull 7 9)))


(flag-show (opponent) 0)


(map
 (lambda
   (chr-new (opponent) (car $0) (cdr $0) 'hostile 0))
 '((3 . 14)
   (4 . 14)
   (5 . 14)
   (3 . 12)
   (4 . 12)
   (5 . 12)
   (7 . 12)))


(weather 3)
