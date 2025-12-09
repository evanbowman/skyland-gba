;;;
;;; neutral/1/8_1_sylph.lisp
;;;


(dialog "You encounter a Sylph civilian vessel. The occupant is fleeing...")


(opponent-init -3 'neutral)

(island-configure
 (opponent)
 '((power-core 2 13)
   (bronze-hull 1 13)
   (bronze-hull 1 14)))

(terrain-set (opponent) 4)



(defn on-converge ()
  (dialog "<c:Sylph Artisan:46>They're calling everyone to military service. Everyone! <B:0> I'm a crystal sculptor. I know nothing of weapons. But the Conclave says 'all resources must be optimized for defense.' <B:0> They're converting our workshops into armories. Our archives into barracks. <B:0> I... I couldn't do it. I left.")

  (defn on-dialog-closed ()
    (dialog "He seems harmless, invite him aboard?")

    (dialog-await-binary-q-w/lore "Join up with us!" "Best of luck…"
                                  '(("Why leave now?" . "<c:Sylph Artisan:46>Three days ago, they ordered me to disassemble the Hall of Echoes, a resonance chamber that took my mentor forty years to tune. <B:0> 'Salvage the crystals for weapon cores,' they said. <B:0> I asked if we were at least recording the acoustic patterns, preserving the knowledge. <B:0> The overseer told me: 'We can rebuild archives after we survive. We cannot rebuild if we are dead.' <B:0> [pause] <B:0> But what survives if we destroy everything that made us worth saving?")))
    (setq on-dialog-closed '()))

  (setq on-converge nil))


(chr-new (opponent) 0 14 'neutral '((icon . 46) (race . 4)))


(defn on-dialog-accepted ()
  (let ((temp (chr-slots (player)))
        (join (lambda (txt)
                (adventure-log-add 53 '())
                (dialog txt))))
    (if temp
        (progn
          (setq temp (get temp (choice (length temp))))
          (chr-new (player) (car temp) (cdr temp) 'neutral '((icon . 46) (race . 0)))
          (chr-del (opponent) 0 14)
          (if (or (equal (choice 2) 1) (< (coins) 600))
              (join "The deserter joined your crew!")
              (progn
                (coins-set (- (coins) 600))
                (join "The deserter joined your crew. Hungry, he ate 600@ of your food supplies!"))))
        (progn
          (dialog "Sadly, there's no room...")
          (defn on-dialog-closed ()
            (dialog "<c:Sylph Artisan:46>No room in your castle? Hold on, I've got some supplies, I'll help out...")
            (defn on-dialog-closed ()
              (alloc-space 'ladder)
              (sel-input 'ladder
                         "Place ladder (1x2):"
                         (lambda (isle x y)
                           (sound "build0")
                           (room-new (player) `(ladder ,x ,y))
                           (chr-del (opponent) 0 14)
                           (chr-new (player) x (+ 1 y) 'neutral '((icon . 46) (race . 0)))
                           (dialog "<c:Sylph Artisan:46>Thanks! I'll try to help out however I can!")
                           (defn on-dialog-closed ()
                             (join "The deserter joined your crew!")
                             (defn on-dialog-closed ()
                               (exit-with-commentary "welcomes_sylph_artisan"))))))))))
  (exit))


(defn on-dialog-declined ()
  (exit))
