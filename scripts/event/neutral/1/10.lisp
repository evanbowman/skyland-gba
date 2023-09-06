
(dialog "<b:/scripts/misc/img/explorer.skg>You come across an explorer's balloon, floating gently in the breeze. You adjust your engines to keep pace with it...")

(opponent-init -3 'neutral)

(island-configure
 (opponent)
 '((balloon 0 10)))

(terrain (opponent) -3)

(chr-new (opponent) 1 14 'neutral 0)


(defn on-converge
  (dialog
   "<c:explorer:22>Hey there! You know, looks like we're going in the same direction! How about we join up?")

  (setq on-dialog-closed
        (lambda
          (dialog "He seems harmless, invite him aboard?")
          (dialog-await-y/n)
          (setq on-dialog-closed '())))
  (setq on-converge nil))


(defn on-dialog-accepted

  (let ((temp (chr-slots (player)))
        (join (lambda
                (adventure-log-add 53 '())
                (dialog $0))))
    (if temp
        (progn
          (setq temp (get temp (choice (length temp))))
          (chr-new (player) (car temp) (cdr temp) 'neutral nil)
          (chr-del (opponent) 1 14)
          (if (or (equal (choice 2) 1) (< (coins) 600))
              (join "The explorer joined your crew!")
            (progn
              (coins-set (- (coins) 600))
              (join "The explorer joined your crew. Hungry, he ate 600@ of your food supplies!"))))
      (progn
        (dialog "Sadly, there's no room...")
        (defn on-dialog-closed
          (dialog "<c:explorer:22>No room in your castle? Hold on, I've got some supplies, I'll help out...")
          (defn on-dialog-closed
            (while (< (length (construction-sites (player) '(1 . 2))) 1)
              (terrain (player) (+ (terrain (player)) 1)))
            (sel-input 'ladder
                       "Place ladder (1x2):"
                       (lambda
                         (sound "build0")
                         (room-new (player) `(ladder ,$1 ,$2))
                         (chr-del (opponent) 1 14)
                         (chr-new (player) $1 (+ 1 $2) 'neutral nil)
                         (dialog "<c:explorer:22> Thanks! I'll try to help out however I can!")
                         (defn on-dialog-closed
                           (join "The explorer joined your crew!")
                           (setq on-dialog-closed nil)
                           (exit)))))))))
  (exit))


(defn on-dialog-declined
  (exit))
