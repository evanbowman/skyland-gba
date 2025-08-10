;;;
;;; util/debrief.lisp
;;;
;;; The game evaluates this script in adventure mode after leaving a level.
;;;
;;; After a level ends, the game will construct a dialog sequence using the
;;; debrief-strs variable. If you would like a dialog sequence to run after a
;;; level, push some stuff to debrief-strs.
;;;


(defn/temp gen (cb n)
  (let ((i 0)
        (ret nil))
    (while (< i n)
      (setq ret (cons (cb) ret))
      (+= i 1))
    ret))

(defn/temp shuffle (lat)
  (let ((tmp (map cons lat (gen (curry choice 10000) (length lat)))))
    (setq tmp (sort tmp (lambda (v1 v2)
                          (< (cdr v1) (cdr v2)))))
    (map car tmp)))


(defn/temp get-icon (chr)
  (lookup 'icon (cddr chr)))


(lambda (scenario)
  (let ((crew (filter get-icon (chrs (player))))
        (key scenario))
    (setq crew (map get-icon (slice (shuffle crew) 0 (choice 4))))
    (setq debrief-strs
          (map (lambda (icon)
                 (read-ini "/scripts/data/character_inter.ini"
                           (format "character_%" icon)
                           key))
               crew))))
