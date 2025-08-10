;;;
;;; quest_marker/atomics.lisp
;;;


(dialog "You arrive at the coordinates of the hidden stash of atomics...")


(opponent-init 9 'neutral)

(island-configure
 (opponent)
 '((hull 0 9) (hull 0 10) (hull 0 11) (hull 0 14) (hull 0 12) (hull 0 8) (hull 0 13) (hull 1 13) (hull 1 14) (cloak 1 12) (hull 1 11) (hull 1 7) (hull 1 10) (cloak 1 8) (hull 1 9) (hull 2 14) (hull 2 7) (warhead 2 12) (hull 2 9) (hull 2 11) (hull 2 10) (hull 2 8) (hull 3 7) (war-engine 3 10) (hull 3 9) (hull 3 14) (hull 3 8) (cloak 4 14) (cloak 4 9) (hull 4 7) (hull 5 14) (hull 5 9) (hull 5 7) (hull 5 8) (hull 6 14) (hull 6 7) (hull 6 10) (hull 6 8) (hull 6 11) (hull 6 9) (cloak 7 12) (hull 7 13) (hull 7 11) (hull 7 9) (hull 7 7) (cloak 7 8) (hull 7 10) (hull 7 14) (hull 8 9) (hull 8 10) (hull 8 11) (hull 8 12) (hull 8 13) (hull 8 8) (hull 8 14)))


(defn on-converge ()
  (dialog "You arrive, but the princess looks troubled...")

  (defn on-dialog-closed ()
    (dialog "<c:Warrior Princess of E. Cay:28>Since shortly after we departed, I haven't been able to contact anyone back home. <B:0> I have instructions, if our kingdom is overrun, to activate the atomics and use them to avenge my people's deaths. Please accept this:")
    (defn on-dialog-closed ()
      (dialog "[You write down the launch codes...]")
      (defn on-dialog-closed ()
        (dialog "<c:Warrior Princess of E. Cay:28>Now, be very careful when moving them...")
        (defn on-dialog-closed ()
          (foreach (lambda (r)
                     (when (equal (car r) 'cloak)
                       (room-del (opponent) (get r 1) (get r 2))))
                   (rooms (opponent)))

          (alloc-space 'warhead)

          (sel-input 'warhead
                     "Place weapon (1x2)"
                     (lambda (isle x y)
                       (foreach (lambda (r)
                                  (when (equal (car r) 'warhead)
                                    (room-del (opponent) (get r 1) (get r 2))))
                                (rooms (opponent)))
                       (room-new (player) (list 'warhead x y))
                       (adventure-log-add 64 '())
                       (sound "build0")
                       (dialog "You retrieved an atomic missile! There were others, but only one was still functioning.")
                       (defn on-dialog-closed ()
                         (dialog "<c:Warrior Princess of E. Cay:28>Those goblins will be sorry they crossed us!")
                         (setq on-dialog-closed exit)))))))))
