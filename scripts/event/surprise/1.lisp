;;;
;;; Mimic isle event
;;;


(let ((bonus (get '(1500 2500 4500 6000) (zone))))
  (dialog "An eerie wind blows through the night... <B:0> "
          (format "An alarm beacon wakes your crew. You investigate, and discover %@! " bonus)
          "<B:0> But just then, a trap springs... and you find yourself facing a devious opponent... the dreaded mimic ship!")

  (coins-add bonus))


(weather 7)


(opponent-init 7 'hostile)
(run-util-script "clone-isle" (player) (opponent))

(when (< (difficulty) 2)
  (foreach (lambda (room)
             ;; Don't make the player fight a ship with atomic warheads unless
             ;; we're in hard mode.
             (if (equal (car room) 'warhead)
                 (room-mut (opponent) (get room 1) (get room 2) 'missile-silo)))
           (rooms (opponent))))

(achieve 13)
