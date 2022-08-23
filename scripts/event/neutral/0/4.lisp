;;;
;;; neutral/0/4.lisp
;;;


(dialog "You discover the wreckage of a goblin raid...")


(opponent-init 8 'neutral)

(island-configure
 (opponent)
 '((hull 0 14)
   (hull 0 13)
   (hull 0 12)
   (masonry 1 14)
   (masonry 1 13)
   (masonry 2 14)
   (plundered-room 2 12)
   (masonry 3 14)
   (plundered-room 3 12)
   (masonry 4 14)
   (power-core 5 13)
   (rocket-bomb 7 12)))


(defn on-converge
  (dialog
   "The island seems thoroughly ransacked... but the pirates inexplicably "
   "left behind a weapon. Haul it aboard?")
  (dialog-await-y/n))


(defn on-dialog-accepted
  (while (not (construction-sites (player) '(1 . 3)))
    (terrain (player) (+ (terrain (player)) 1)))
  (room-del (opponent) 7 12)
  (sel-input '(1 . 3)
             "Pick a slot (1, 3)"
             (lambda
               (room-new (player) (list 'rocket-bomb $1 $2))
               (dialog "A great addition!")
               (setq on-dialog-closed exit))))

(defn on-dialog-declined
  (dialog "Huh!? Who doesn't want free stuff? Suit yourself...")
  (setq on-dialog-closed exit))
