;; The regression program in developer mode runs the unit tests in misc, and
;; then executes each tutorial, checking the tutorial results with the numbered
;; scripts in this directory.

(defn assert-eq [2]
  (when (not (equal $0 $1))
    (error (format "failure! expected % not equal %" $0 $1))))
