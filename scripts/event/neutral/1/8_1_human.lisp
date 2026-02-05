;;;
;;; neutral/1/8_1_human.lisp
;;;


(dialog "<b:/scripts/data/img/explorer.img.bin>You come across an explorer's balloon, floating gently in the breeze. You adjust your engines to keep pace with it...")

(opponent-init -3 'neutral)

(island-configure
 (opponent)
 '((balloon 0 10)))

(terrain-set (opponent) -3)


(chr-new (opponent) 1 14 'neutral '((icon . 22) (race . 0)))



(defn on-converge ()
  (setq on-converge nil)
  (await (dialog* "<c:Explorer:22>Hey there! You know, looks like we're going in the same direction! How about we join up?"))

  (dialog "He seems harmless, invite him aboard?")
  (dialog-setup-binary-q-w/lore "Welcome aboard!" "Sorry, but no."
                                '(("Let's chat…" . "<c:Explorer:22>I'm obsessed with finding new islands! When I find one, I mark it with a signal beacon. That's how you can find islands on your sky chart! Neat huh? <B:0> Anyway, can I come aboard?"))))



(defn/temp join-crew (xy messages)
  (adventure-log-add 53 '())
  (chr-new (player) (car xy) (cdr xy) 'neutral '((icon . 22) (race . 0)))
  (chr-del (opponent) 1 14)
  (apply dialog-sequence messages)
  (exit-with-commentary "welcomes_explorer"))


(defn/temp join-has-space (slots)
  (let ((xy (sample slots)))
    (if (or (equal (choice 2) 1) (< (coins) 600))
        (join-crew xy '("The explorer joined your crew!"))
        (progn
          (coins-set (- (coins) 600))
          (join-crew xy '("The explorer joined your crew. Hungry, he ate 600@ of your food supplies!"))))))


(defn/temp join-crowded ()
  (await (dialog* "Sadly, there's no room..."))
  (await (dialog* "<c:Explorer:22>No room in your castle? Hold on, I've got some supplies, I'll help out..."))
  (alloc-space 'ladder)
  (let ((xy (await (sel-input* 'ladder
                               "Place ladder (1x2):"))))
    (sound "build0")
    (room-new (player) `(ladder ,(car xy) ,(cdr xy)))
    (join-crew xy '("<c:explorer:22>Thanks! I'll try to help out however I can!"
                    "The explorer joined your crew!"))))


(defn on-dialog-accepted ()
  (let ((slots (chr-slots (player))))
    (if slots
        (join-has-space slots)
        (join-crowded))))


(defn on-dialog-declined ()
  (exit))
