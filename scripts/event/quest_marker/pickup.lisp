

(dialog "At last, the pickup address...")


(opponent-init 7 'neutral)


(island-configure
 (opponent)
 '((power-core 5 13)))

(chr-new (opponent) 1 14 'neutral 0)
(chr-new (opponent) 2 14 'neutral 0)
(chr-new (opponent) 3 14 'neutral 0)


(defn on-dialog-closed
  (setq on-dialog-closed exit)
  (map
   (lambda
     (let ((slot (chr-slots (player))))
       (if (not slot)
           (let ((s (construction-sites (player) '(1 . 2))))
             (if s
                 (progn
                   (room-new (player) (list 'cargo-bay (car (car s)) (cdr (car s))))
                   (setq slot (chr-slots (player)))))))
       (if slot
           (progn
             (chr-new (player) (car (car slot)) (cdr (car slot)) 'neutral 0)
             (chr-del (opponent) (car $0) (cdr $0))))))
   '((1 . 14) (2 . 14) (3 . 14)))

  (if (equal 0 (length (chrs (opponent))))
      (dialog "The passengers joined your crew!")
    (if (< 3 (length (chrs (opponent))))
        (dialog "Some of the passengers joined your crew!")
      (dialog "You'd like to invite them aboard, but there seems to be no room..."))))


(defn on-converge
  (dialog
   "<c:passengers:11>We were starting to wonder if anyone would show up! How about we join up, it'll be safer to travel together!"))
