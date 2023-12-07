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
   (torch 0 11)
   (masonry 1 14)
   (masonry 1 13)
   (masonry 2 14)
   (plundered-room 2 12)
   (torch 2 10)
   (masonry 3 14)
   (plundered-room 3 12)
   (masonry 4 14)
   (power-core 5 13)))

(flag-show (opponent) 7)


(secret
 0 14
 "Goblins from the surface! Now they too inhabit the skies... nowhere is safe...")


(let ((wpn 'rocket-bomb)
      (pos '(7 . 12)))

  (when (equal (choice 7) 0)
    (setq wpn 'splitter)
    (setq pos '(7 . 13))
    (terrain-set (opponent) 9))

  (room-new (opponent) (list wpn (car pos) (cdr pos)))


  (defn on-fadein [0]
    (fire-new (opponent) 0 11)
    (fire-new (opponent) 2 10)
    (setq on-fadein nil))


  (defn on-converge [0]
    (dialog
     "The island seems thoroughly ransacked... but the pirates inexplicably "
     "left behind a weapon. Haul it aboard?")
    (dialog-await-y/n)
    (setq on-converge nil))


  (defn on-dialog-accepted [0]
    (alloc-space wpn)
    (room-del (opponent) (car pos) (cdr pos))
    (sel-input wpn
               (format "Pick a slot (%x%)" (car (rinfo 'size wpn)) (cdr (rinfo 'size wpn)))
               (lambda
                 (room-new (player) (list wpn $1 $2))
                 (sound "build0")
                 (cond
                  ((equal wpn 'rocket-bomb)
                   (dialog "Like a missile-silo, but starts fires! A useful addition!"))
                  ((equal wpn 'splitter)
                   (dialog "Wow, a very powerful weapon! <B:0> You're lucky to have found this..."))
                  (true
                   (dialog "...")))

                 (setq on-dialog-closed exit)))
    (adventure-log-add 9 '()))

  (defn on-dialog-declined [0]
    (dialog "Huh!? Who doesn't want free stuff? Suit yourself...")
    (adventure-log-add 8 '())
    (setq on-dialog-closed exit)))
