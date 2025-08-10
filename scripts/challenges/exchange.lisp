;;;
;;; challenges/exchange.lisp
;;;


(defn challenge-hint ()
  (dialog "Sorry, no hints..."))


(defn on-victory ()
  (challenge-complete 10))


(defn challenge-hint ()
  (dialog "Are you sure you want a hint?")
  (dialog-await-y/n)

  (defn on-dialog-accepted ()
    (dialog "Hint: Transporters can remove crewmembers from enemy castles..."))

  (setq on-dialog-declined (lambda ())))



(terrain-set (player) 7)
(island-configure
 (player)
 '((radar 0 13) (masonry 1 14 0) (transporter 1 12) (transporter 1 9) (solar-cell 1 8) (masonry 1 11 0) (masonry 2 14 0) (masonry 2 11 0) (power-core 2 12) (hull 3 10) (masonry 3 11 0) (masonry 3 14 0) (masonry 4 14 0) (masonry 4 13 0) (masonry 4 12 0) (hull 4 10) (masonry 4 11 0) (hull 5 11) (hull 5 10) (masonry 5 14 0) (masonry 5 13 0) (masonry 5 12 0) (hull 5 9) (hull 5 8) (hull 6 11) (hull 6 12) (hull 6 13) (hull 6 14)))


(map
 (lambda (xy)
   (chr-new (player) (first xy) (second xy) 'neutral nil))
 '((3 . 13)
   (2 . 13)))



(coins-add 300)


(flag-show (player) 0)


(opponent-init 10 'hostile)


(island-configure
 (opponent)
 '((forcefield 0 13) (forcefield 0 12) (forcefield 0 14) (forcefield 1 11) (forcefield* 1 13) (forcefield 1 12) (forcefield 1 14) (forcefield 2 13) (hull 2 10) (forcefield* 2 12) (forcefield* 2 14) (forcefield 2 11) (hull 3 9) (hull 3 10) (flak-gun 3 13) (forcefield 3 12) (hull 3 11) (flak-gun 3 14) (hull 4 11) (hull 4 10) (hull 4 9) (hull 4 12) (power-core 5 11) (hull 5 10) (power-core 5 13) (hull 5 9) (hull 6 10) (hull 6 9) (hull 7 10) (hull 7 11) (hull 7 12) (power-core 7 13) (hull 7 9) (hull 8 10) (power-core 8 11) (hull 8 9) (hull 9 10) (ladder 9 13) (hull 9 9)))


(weather-set 4)


(map
 (lambda (xy)
   (chr-new (opponent) (first xy) (second xy) 'hostile 0))
 '((6 . 14)
   (5 . 14)))
