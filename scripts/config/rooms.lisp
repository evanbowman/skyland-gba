;;;
;;; rooms.lisp
;;;

;; (1) health
;; (2) cost
;; (3) power consumption

(configure-rooms
               ; (1) (2)  (3)
 '((hull         240 300  0)
   (forcefield   240 300  40)
   (ion-fizzler  40  100  20)
   (cannon       200 1000 34)
   (ion-cannon   150 800  30)
   (flak-gun     125 2000 34)
   (missile-silo 200 1400 30)
   (decimator    140 3300 160)
   (stairwell    60  500  5)
   (bulkhead-door 100 500  10)
   (workshop     60  2000 10)
   (infirmary    60  1000 10)
   (power-core   60  3000 -150)
   (reactor      140 4200 -400)
   (foundry      80  4500 30)
   (radar        30  300  80)
   (transporter  100 999  40)
   (replicator   80  2500 80)
   (drone-bay    200 2100 36)
   (cargo-bay    60  300  10)
   (arc-gun      240 1400 40)
   (energized-hull 380 450 20)
   (plundered-room 20  30  0)))
