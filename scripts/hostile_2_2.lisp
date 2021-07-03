;;;
;;; hostile_2_2.lisp
;;;


(init-opponent 7 'hostile)


;; (configure-player
;;  (player)
;;  '((power-core 1 13)
;;    (radar 3 13)))

;; (add-chr (player) 2 14)


(configure-player
 (opponent)
 '((power-core 4 13)
   (power-core 4 11)
   (stairwell 6 11)
   (transporter 3 13)
   (transporter 3 11)
   (transporter 2 11)
   (infirmary 1 13)
   (hull 0 14)
   (hull 0 13)
   (hull 0 12)
   (hull 0 11)
   (missile-silo 1 11)
   (hull 2 10)
   (hull 3 10)
   (hull 5 10)
   (hull 4 10)
   (hull 6 10)
   (hull 2 9)
   (hull 3 9)))


(show-flag (opponent))

(add-coins 10000)

(map
 (lambda
   (add-hostile-chr (opponent) (car (arg 0)) (cdr (arg 0))))
 '((4 . 14)
   (5 . 14)
   (4 . 12)
   (5 . 12)
   (6 . 14)
   (6 . 13)))
