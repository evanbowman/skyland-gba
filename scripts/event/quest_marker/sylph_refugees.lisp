;;;
;;; quest_marker/sylph_refugees.lisp
;;;


(dialog "You arrive at Research Platform Delta. The facility drifts silently, emergency lights pulsing across its hull...")


(opponent-init 7 'neutral)
(island-configure
 (opponent)
 '((power-core 5 13)))


(chr-new (opponent) 1 14 'neutral '((race . 4)))
(chr-new (opponent) 2 14 'neutral '((race . 4)))
(chr-new (opponent) 3 14 'neutral '((race . 4)))


(defn on-converge ()
  (dialog
   "<c:Sylph Facilities Staff:53>Good, you're here. <B:0> This is the third facility evacuation this month. Transport resources are stretched thin across all sectors. <B:0> The lead researchers departed on the priority vessel - their work continues elsewhere. We stayed to secure the equipment. <B:0> The system works, but... it's good to see an independent operator. 1500@ authorized.")
  (coins-add 1500))


(defn on-dialog-closed ()
  (setq on-dialog-closed exit)

  (map
   (lambda (xy)
     (let ((slot (chr-slots (player))))
       (if (not slot)
           (let ((s (construction-sites (player) '(1 . 2))))
             (if s
                 (progn
                   (room-new (player) (list 'ladder (caar s) (cdr (car s))))
                   (setq slot (chr-slots (player)))))))
       (if slot
           (progn
             (chr-new (player)
                      (caar slot)
                      (cdr (car slot))
                      'neutral
                      '((race . 4) (icon . 53)))
             (chr-del (opponent) (get xy 0) (get xy 1))))))
   '((1 14) (2 14) (3 14)))

  (adventure-log-add 25 '())

  (if (equal 0 (length (chrs (opponent))))
      (dialog "The Sylph facilities staff boarded your vessel, bringing with them carefully packed equipment cases.")
    (if (< 3 (length (chrs (opponent))))
        (dialog "Some of the staff came aboard. The others will wait for another vessel.")
      (dialog "The staff are ready to board, but you don't have room for them."))))
