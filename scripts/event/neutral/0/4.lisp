;;;
;;; neutral/0/4.lisp
;;;


(dialog "<b:/scripts/misc/img/wreckage.skg>You discover the wreckage of a goblin raid...")


(opponent-init 8 'neutral)

(island-configure
 (opponent)
 '((hull 0 14)
   (hull 0 13)
   (hull 0 12)
   (torch 0 11)
   (masonry 1 14)
   (masonry 1 13)
   (masonry 2 14)
   (plundered-room 2 12)
   (torch 2 10)
   (masonry 3 14)
   (plundered-room 3 12)
   (masonry 4 14)
   (power-core 5 13)
   (rocket-bomb 7 12)))

(flag-show (opponent) 7)


(secret
 0 14
 "Goblins from the surface! Now they too inhabit the skies... nowhere is safe...")


(defn on-fadein
  (fire-new (opponent) 0 11)
  (fire-new (opponent) 2 10)
  (setq on-fadein nil))


(defn on-converge
  (dialog
   "The island seems thoroughly ransacked... but the pirates inexplicably "
   "left behind a weapon. Haul it aboard?")
  (dialog-await-y/n)
  (setq on-converge nil))


(defn on-dialog-accepted
  (while (not (construction-sites (player) '(1 . 3)))
    (terrain (player) (+ (terrain (player)) 1)))
  (room-del (opponent) 7 12)
  (sel-input 'rocket-bomb
             "Pick a slot (1x3)"
             (lambda
               (room-new (player) (list 'rocket-bomb $1 $2))
               (syscall "sound" "build0")
               (dialog "Like a missile-silo, but starts fires! A useful addition!")
               (setq on-dialog-closed exit)))
  (adventure-log-add 9 '()))

(defn on-dialog-declined
  (dialog "Huh!? Who doesn't want free stuff? Suit yourself...")
  (adventure-log-add 8 '())
  (setq on-dialog-closed exit))
