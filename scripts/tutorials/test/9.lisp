;;;
;;; characters.lisp
;;;

(assert-eq
 (rooms (player))
 '((power-core 1 13) (infirmary 1 11) (workshop 3 13) (workshop 3 11) (stairwell 5 11)))

(assert-eq
 (chrs (player))
 '((2 12 (dr . 30) (sc . 45) (id . 1))))
