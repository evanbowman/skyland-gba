;;;
;;; neutral/2/1_alt1.lisp
;;;


(dialog "A damaged fortress floats into view. The residents do not respond to your radio signals.")


(opponent-init 8 'neutral)


(island-configure
 (opponent)
 '((hull 0 12 56)
   (hull 0 11)
   (hull 0 13)
   (hull 0 14 16)
   (hull 1 11)
   (hull 1 12)
   (hull 1 14 72)
   (hull 1 13)
   (hull 2 14 200)
   (hull 3 10)
   (hull 3 14)
   (transporter 3 12)
   (hull 4 9)
   (reactor 4 11)
   (hull 4 10)
   (masonry 4 14 3)
   (hull 5 10)
   (masonry 5 14 3)
   (hull 6 14)
   (ladder 6 12)
   (infirmary 6 10)
   (hull 6 8)
   (hull 7 14)
   (hull 7 12)
   (windmill 7 13)
   (hull 7 9)
   (hull 7 8)))


(map (lambda (xy)
       (chr-new (opponent) (first xy) (second xy) 'neutral 0))
     '((0 . 10)
       (1 . 10)
       (6 . 7)))


(defn on-converge ()
  (dialog "The island's radio appears to be broken. <B:0> Three survivors signal to you that they'd like to come aboard, but it's not clear whether they can be trusted. <B:0> Invite survivors aboard?")

  (dialog-setup-y/n)
  (setq on-converge nil))


(defn/temp move-chrs (mode)
  (let ((ch '((0 . 10) (1 . 10) (6 . 7))))
    (while (and ch (chr-slots (player)))
      (let ((sl (chr-slots (player))))
        (chr-del (opponent) (caar ch) (cdar ch))
        (setq ch (cdr ch))
        (chr-new (player) (caar sl) (cdar sl) mode 0)))
    (while (and ch (chr-slots (opponent)))
      (let ((sl (chr-slots (opponent))))
        (chr-del (opponent) (caar ch) (cdar ch))
        (setq ch (cdr ch))
        (chr-new (opponent) (caar sl) (cdar sl) mode 0)))))



(defn/temp join-bad ()
  (move-chrs 'hostile)
  (chr-new (opponent) 4 13 'hostile 0)
  (chr-new (opponent) 5 13 'hostile 0)
  (room-new (opponent) '(mycelium 3 11))
  (room-new (opponent) '(mycelium 6 9))
  (room-mut (opponent) 0 12 'arc-gun)
  (room-mut (opponent) 0 14 'arc-gun)
  (opponent-mode 'hostile)
  (foreach (lambda (room)
             (if (equal (car room) 'hull)
                 (room-mut (opponent)
                           (get room 1)
                           (get room 2)
                           'mirror-hull)))
           (rooms (opponent)))
  (await (dialog* "The survivors turned out to be vicious goblins, and their island is not as defenseless as it initially appeared..."))
  (await (dialog* "<c:Goblin:2>Die "
                  (cond
                    ((equal (faction) 'goblin) "Traitorsss")
                    ((equal (faction) 'human) "Humansss")
                    ((equal (faction) 'sylph) "Sssylph ssscum"))
                  "!")))


(defn/temp join-good ()
  (move-chrs 'neutral)
  (await (if (chrs (opponent))
             (dialog* "Some of the survivors joined your crew!")
             (dialog* "The survivors joined your crew!")))
  (await (dialog* "..."))
  (exit))


(let ((bad (choice 2)))
  (defn on-dialog-accepted ()
    (cond
     (bad (join-bad))
     (true (join-good)))))


(setq on-dialog-declined exit)
