;;;
;;; resurrect.lisp
;;;
;;; Resurrect a snapshot of one of the player's previously-destroyed castles.
;;;


;; Look into the array of snapshots per zone
(set 'temp (get snapshots (zone)))


;; Pick a random snapshot
(set 'temp (get temp (cr-choice (length temp))))


;; Find the highest x-value in the room data (We want to mirror the room over
;; the y-axis).
(set 'max-x 0)
(map (lambda
       (if (> (get (arg 0) 1) max-x)
           (set 'max-x (get (arg 0) 1))))
     temp)


;; Flip the room (the enemy must face the player)
(set 'temp
     (map (lambda
            (list (get (arg 0) 0)
                  (- highest (get (arg 0) 1))
                  (get (arg 0) 2)))
          temp))
