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
   (power-core   60  3000 -150)
   (reactor      100 4200 -380)
   (cannon       200 1000 34)
   (ion-cannon   150 800  30)
   (flak-gun     125 2000 34)
   (missile-silo 200 1400 30)
   (stairwell    60  500  5)
   (bulkhead     100 500  10)
   (workshop     60  2000 10)
   (infirmary    60  1000 10)
   (radar        30  300  64)
   (transporter  100 999  40)
   (replicator   80  2500 80)
   (drone-bay    200 2100 36)
   (decimator    140 3600 80)
   (foundry      80  3000 50)
   (plundered-room 20 30 0)))
