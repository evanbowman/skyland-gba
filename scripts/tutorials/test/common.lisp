;;;
;;; common.lisp
;;;
;;; The regression program in developer mode runs the unit tests in misc, and
;;; then executes each tutorial, checking the tutorial results with the numbered
;;; scripts in this directory.


(defn assert-eq (lhs rhs)
  (when (not (equal lhs rhs))
    (error (format "failure! expected % not equal %" lhs rhs))))
