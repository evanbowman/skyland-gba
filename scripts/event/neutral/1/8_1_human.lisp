;;;
;;; neutral/1/8_1_human.lisp
;;;


(dialog "<b:/scripts/data/img/explorer.img.bin>You come across an explorer's balloon, floating gently in the breeze. You adjust your engines to keep pace with it...")

(opponent-init -3 'neutral)

(island-configure
 (opponent)
 '((balloon 0 10)))

(terrain-set (opponent) -3)



(defn on-converge ()
  (dialog "<c:Explorer:22>Hey there! You know, looks like we're going in the same direction! How about we join up?")

  (setq on-dialog-closed
        (lambda ()
          (dialog "He seems harmless, invite him aboard?")

          (dialog-await-binary-q-w/lore "Welcome aboard!" "Sorry, but no."
                                        '(("Let's chat…" . "<c:Explorer:22>I'm obsessed with finding new islands! When I find one, I mark it with a signal beacon. That's how you can find islands on your sky chart! Neat huh? <B:0> Anyway, can I come aboard?")))

          (setq on-dialog-closed '())))
  (setq on-converge nil))


(chr-new (opponent) 1 14 'neutral '((icon . 22) (race . 0)))


(defn on-dialog-accepted ()
  (let ((temp (chr-slots (player)))
        (join (lambda (txt)
                (adventure-log-add 53 '())
                (dialog txt))))
    (if temp
        (progn
          (setq temp (get temp (choice (length temp))))
          (chr-new (player) (car temp) (cdr temp) 'neutral '((icon . 22) (race . 0)))
          (chr-del (opponent) 1 14)
          (if (or (equal (choice 2) 1) (< (coins) 600))
              (join "The explorer joined your crew!")
              (progn
                (coins-set (- (coins) 600))
                (join "The explorer joined your crew. Hungry, he ate 600@ of your food supplies!"))))
        (progn
          (dialog "Sadly, there's no room...")
          (defn on-dialog-closed ()
            (dialog "<c:Explorer:22>No room in your castle? Hold on, I've got some supplies, I'll help out...")
            (defn on-dialog-closed ()
              (alloc-space 'ladder)
              (sel-input 'ladder
                         "Place ladder (1x2):"
                         (lambda (isle x y)
                           (sound "build0")
                           (room-new (player) `(ladder ,x ,y))
                           (chr-del (opponent) 1 14)
                           (chr-new (player) x (+ 1 y) 'neutral '((icon . 22) (race . 0)))
                           (dialog "<c:explorer:22>Thanks! I'll try to help out however I can!")
                           (defn on-dialog-closed ()
                             (join "The explorer joined your crew!")
                             (defn on-dialog-closed ()
                               (exit-with-commentary "welcomes_explorer"))))))))))
  (exit))


(defn on-dialog-declined ()
  (exit))
