
(dialog
 ;"<b:/scripts/data/img/burning_isle.img.bin> "
 "You arrive at the coordinates of the hidden stash of atomics!")


(opponent-init 9 'neutral)

(island-configure
 (opponent)
 '((hull 0 9) (hull 0 10) (hull 0 11) (hull 0 14) (hull 0 12) (hull 0 8) (hull 0 13) (hull 1 13) (hull 1 14) (cloak 1 12) (hull 1 11) (hull 1 7) (hull 1 10) (cloak 1 8) (hull 1 9) (hull 2 14) (hull 2 7) (warhead 2 12) (hull 2 9) (hull 2 11) (hull 2 10) (hull 2 8) (hull 3 7) (war-engine 3 10) (hull 3 9) (hull 3 14) (hull 3 8) (cloak 4 14) (cloak 4 9) (hull 4 7) (hull 5 14) (hull 5 9) (hull 5 7) (hull 5 8) (hull 6 14) (warhead 6 12) (hull 6 7) (hull 6 10) (hull 6 8) (hull 6 11) (hull 6 9) (cloak 7 12) (hull 7 13) (hull 7 11) (hull 7 9) (hull 7 7) (cloak 7 8) (hull 7 10) (hull 7 14) (hull 8 9) (hull 8 10) (hull 8 11) (hull 8 12) (hull 8 13) (hull 8 8) (hull 8 14)))


(defn on-converge ()
  (dialog "Just as you arrive, you receive an emergency transmission...")

  (defn on-dialog-closed ()
    (dialog "<c:king:27> There's been a change in plans. We're being overrun by goblins! Do not bring the arsenal back here! <B:0> My son has been sending me reports, and believes that you really are trustworthy. <B:0> Here are the activation codes:")
    (defn on-dialog-closed ()
      (dialog "[You write down the missile launch codes...]")
      (defn on-dialog-closed ()
        (dialog "<c:king:27> Hopefully you will never need to use them...")
        (defn on-dialog-closed ()
          (foreach (lambda (r)
                     (when (equal (car r) 'cloak)
                       (room-del (opponent) (get r 1) (get r 2))))
                   (rooms (opponent)))

          ;; NOTE: I was too lazy to create two input selections. So I alloc
          ;; space for a 2x2 room, which is enough space for to 1x2 warhead
          ;; blocks.
          (alloc-space 'incinerator)

          (sel-input 'incinerator
                     "Place weapons (2x2)"
                     (lambda (isle x y)
                       (foreach (lambda (r)
                                  (when (equal (car r) 'warhead)
                                    (room-del (opponent) (get r 1) (get r 2))))
                                (rooms (opponent)))
                       (room-new (player) (list 'warhead x y))
                       (room-new (player) (list 'warhead (+ x 1) y))
                       (sound "build0")
                       (dialog "You retrieved two atomic missiles!")
                       (defn on-dialog-closed ()
                         (dialog "<c:king:27> We can't hang on much longer; the thing in this storm is truely horrible! Please help put an end to this fighting...")
                         (setq on-dialog-closed exit)))))))))
