;;;
;;; quest_marker/goblin_pickup_0.lisp
;;;


(dialog "You reach the source of the smoke signals...")


(opponent-init 7 'neutral)


(island-configure
 (opponent)
 '((power-core 5 13)))

(chr-new (opponent) 1 14 'neutral '((race . 1)))
(chr-new (opponent) 2 14 'neutral '((race . 1)))
(chr-new (opponent) 3 14 'neutral '((race . 1)))


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
                      '((race . 1)))
             (chr-del (opponent) (get xy 0) (get xy 1))))))
   '((1 14) (2 14) (3 14)))

  (adventure-log-add 25 '())

  (if (equal 0 (length (chrs (opponent))))
      (dialog "The scattered clan members joined your crew!")
    (if (< 3 (length (chrs (opponent))))
        (dialog "Some of the clan members joined your crew!")
      (dialog "You'd like to invite them aboard, but there seems to be no room..."))))


(defn on-converge ()
  (dialog
   "<c:Clan Members:2>The ssstorm nearly had us! Together we'll be ssstronger. Here'sss 1500@ from our clan's reserves.")
  (coins-add 1500))
