;;;
;;; neutral/1/9.lisp
;;;


(dialog "<b:/scripts/data/img/drone_carrier.img.bin> An old drone carrier approaches and activates. Looks like it hasn't met anyone in a long time...")


(opponent-init 7 'neutral)

(island-configure
 (opponent)
 '((canvas 0 12 (20 573964433 1066409474 402563237 -20889608 15 240 126 128))
   (canvas 0 11 (21 465620097 1006670130 1040385 515900280 563456 128))
   (hull 0 14)
   (canvas 0 13 (19 573964433 -2088767473 -2088964 1133181959 17 0 96))
   (hull 1 11)
   (hull 1 14)
   (power-core 1 12)
   (hull 2 14)
   (hull 2 11)
   (canvas 3 12 (24 538345608 26245135 2289987 -20889816 -2088767473 254 4 15 112))
   (drone-bay 3 13)
   (hull 3 14)
   (drone-bay 3 11)
   (hull 4 14)
   (canvas 4 10 (30 1141533696 -2097098739 8917350 1076915736 29889541 806337089 453279853 160 1))
   (drone-bay 5 13)
   (hull 5 14)
   (drone-bay 5 11)
   (canvas 5 10 (27 20709521 -536510248 8577921 1719695440 -939003904 403576384 136 3 0))
   (canvas 5 9 (29 1141533696 541505549 131932160 -297716280 -1854246899 -585105084 1077348352 2))
   (hull 6 14)
   (canvas 6 12 (26 574766080 1149521920 -1995502976 -29294320 -2088767473 552606972 123 0))
   (canvas 6 10 (35 1141768385 -2143612928 8933694 -1035968496 -1976409828 165675452 1093828752 645957664 32 127 48))
   (canvas 6 9 (38 573440145 37755906 14418146 -669057012 589317256 369163492 268436515 243827105 403191680 28 192))
   (canvas 6 8 (30 -1073514496 9503251 -140502260 1094563846 59777203 555759724 1610665990 32 33))))


(secret 6 14 "yes no")

(flag-show (opponent) 4)


(defn on-converge ()
  ;; want drones?
  (dialog "<c:robot:13> 01010111 01100001 01101110 01110100 00100000 01100100 01110010 01101111 01101110 01100101 01110011 00111111?")
  (dialog-await-y/n)

  (defn on-dialog-accepted ()
    ;; less than 2?
    (dialog "<c:robot:13> 00111100 00100000 00110010?")
    (dialog-await-y/n)

    (adventure-log-add 39 '())

    (defn on-dialog-accepted ()
      ;; place one drone bay
      (alloc-space 'drone-bay)

      (sel-input 'drone-bay
                 "Pick a slot (2x1)"
                 (lambda (isle x y)
                   (sound "build0")
                   (room-new (player) `(drone-bay ,x ,y))
                   (dialog "<c:robot:13> 01000010 01111001 01100101!")
                   (exit))))

    (defn on-dialog-declined ()
      ;; place two drone bays

      (alloc-space 'drone-bay)

      (sel-input 'drone-bay
                 "Pick a slot (2x1) (1 of 2)"
                 (lambda (isle x y)
                   (sound "build0")
                   (room-new (player) `(drone-bay ,x ,y))

                   (sel-input 'drone-bay
                              "Pick a slot (2x1) (2 of 2)"
                              (lambda (isle x y)
                                (sound "build0")
                                (room-new (player) `(drone-bay ,x ,y))
                                (dialog "<c:robot:13> 01000010 01111001 01100101!")
                                (exit)))))))

  (setq on-dialog-declined exit))
