;;;
;;; transporter.lisp
;;;

(assert-eq
 (rooms (player))
 '((power-core 1 13) (transporter 3 13) (transporter 4 13)))

(assert-eq
 (rooms (opponent))
 '((power-core 1 13 6) (stairwell 3 11)))

(assert-eq
 (chrs (player))
 '((4 14 (hp . 171) (id . 2))))

(assert-eq
 (chrs (opponent))
 '())
