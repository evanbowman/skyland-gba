;;;
;;; Mimic isle event
;;;


(let ((bonus (get '(1500 2500 3500 4000) (zone))))
  (dialog "An eerie wind blows through the night... <B:0> "
          (format "An alarm beacon wakes your crew. You investigate, and discover %@! " bonus)
          "<B:0> But just then, a trap springs... and you find yourself facing a devious opponent... the dreaded mimic ship!")

  (coins-add bonus))


(weather 7)


(opponent-init 7 'hostile)
(run-util-script "clone-isle" (player) (opponent))


(push-hook 'on-victory (lambda ()
                         (achieve 13)))
