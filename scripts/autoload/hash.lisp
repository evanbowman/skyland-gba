;;;
;;; autoload/hash.lisp
;;;

(lambda (v)
  (cond
    ((and (pair? v) (int? (car v)) (int? (cdr v))) ;; hash for xy coord pair
     (let ((h (+ (* (first v) 374761393) (* (second v) 668265263))))
       (abs (* (bit-xor h (bit-shift-right h 13)) 1274126177))))
    (true
     (error (format "cannot hash %" v)))))
