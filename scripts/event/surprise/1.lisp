;;;
;;; Mimic isle event
;;;

(dialog "An eerie wind blows through the night... <B:0> Your crew awakes to the view of a strangely familiar looking castle...")

(weather 7)


(opponent-init 'hostile 7)
(run-util-script "clone-isle" (player) (opponent))
