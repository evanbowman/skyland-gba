;;;
;;; quest_marker/pickup.lisp
;;;


(dialog "At last, the pickup address...")


(opponent-init 7 'neutral)


(island-configure
 (opponent)
 '((power-core 5 13)))

(chr-new (opponent) 1 14 'neutral 0)
(chr-new (opponent) 2 14 'neutral 0)
(chr-new (opponent) 3 14 'neutral 0)


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
                      nil)
             (chr-del (opponent) (get xy 0) (get xy 1))))))
   '((1 14) (2 14) (3 14)))

  (adventure-log-add 25 '())

  (if (equal 0 (length (chrs (opponent))))
      (dialog "The passengers joined your crew!")
    (if (< 3 (length (chrs (opponent))))
        (dialog "Some of the passengers joined your crew!")
      (dialog "You'd like to invite them aboard, but there seems to be no room..."))))


(defn on-converge ()
  (dialog
   "<c:Passengers:10>We were starting to wonder if anyone would show up! How about we join up, it'll be safer to travel together! Here's 1500@ as a tip.")
  (coins-add 1500))
