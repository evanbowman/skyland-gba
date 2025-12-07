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
                      '((race . 4)))
             (chr-del (opponent) (get xy 0) (get xy 1))))))
   '((1 14) (2 14) (3 14)))

  (adventure-log-add 25 '())

  (if (equal 0 (length (chrs (opponent))))
      (dialog "The researchers boarded your vessel, already cataloging their escape as another data point.")
    (if (< 3 (length (chrs (opponent))))
        (dialog "Some of the researchers came aboard. The others will wait for another vessel.")
      (dialog "The researchers are ready to board, but you don't have room for them."))))


(defn on-converge ()
  (dialog
   "<c:Sylph Researcher:53>You made good time. <B:0> We've already transmitted our readings to the network - the data is safe, at least. <B:0> The Conclave authorized 1500@ for the extraction.")
  (coins-add 1500))
