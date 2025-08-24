;;;
;;; challenges/goliath.lisp
;;;


(coins-add 6000)


(defn on-victory ()
  (challenge-complete 0))


(defn challenge-hint ()
  (dialog "Sorry, no hints for this one."))


(terrain-set (player) 8)
(island-configure
 (player)
 '((power-core 1 13)
   (workshop 1 11)))

(flag-show (player) 0)

(chr-new (player) 1 14 'neutral nil)
(chr-new (player) 2 14 'neutral nil)
(chr-new (player) 1 12 'neutral nil)
(chr-new (player) 2 12 'neutral nil)


;; testing...
;; (eval-file "/scripts/event/neutral/0/0.lisp")


(opponent-init 9 'hostile)


(island-configure
 (opponent)
 '((power-core 3 8)
   (hull 3 12)
   (hull 4 12)
   (power-core 3 13)
   (workshop 3 10)
   (stairwell 6 11)
   (transporter 6 9)
   (transporter 5 13)
   (stairwell 5 9)
   (cannon 1 14)
   (cannon 1 13)
   (cannon 1 12)
   (cannon 1 11)
   (cannon 1 10)
   (cannon 1 9)
   (hull 2 14)
   (hull 2 13)
   (hull 2 12)
   (hull 2 11)
   (hull 2 10)
   (hull 2 9)
   (power-core 7 13)
   (hull 3 6)
   (hull 4 6)
   (hull 3 7)
   (hull 4 7)
   (hull 5 8)
   (hull 6 8)
   (hull 7 12)
   (hull 8 12)))


(chr-new (opponent) 3 14 'hostile 0)


(weather-set weather-id-rain)
