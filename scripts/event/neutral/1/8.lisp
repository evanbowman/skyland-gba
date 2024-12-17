
(dialog (loadstr 9))

(opponent-init -3 'neutral)

(island-configure
 (opponent)
 '((balloon 0 10)))

(terrain-set (opponent) -3)



(defn on-converge ()
  (dialog (loadstr 10))

  (setq on-dialog-closed
        (lambda ()
          (dialog (loadstr 11))

          (dialog-await-binary-q-w/lore "welcome aboard!" "sorry, but no"
                                        (list (cons "let's chat…" (loadstr 12))))

          (setq on-dialog-closed '())))
  (setq on-converge nil))


(let ((params (list
               (cons 'icon (cond
                             ((equal (faction) 'goblin) 36)
                             (true 22)))
               (cons 'race (cond
                             ((equal (faction) 'goblin) 1)
                             (true 0))))))

  (chr-new (opponent) 1 14 'neutral params)

  (defn on-dialog-accepted ()
    (let ((temp (chr-slots (player)))
          (join (lambda (txt)
                  (adventure-log-add 53 '())
                  (dialog txt))))
      (if temp
          (progn
            (setq temp (get temp (choice (length temp))))
            (chr-new (player) (car temp) (cdr temp) 'neutral params)
            (chr-del (opponent) 1 14)
            (if (or (equal (choice 2) 1) (< (coins) 600))
                (join (loadstr 13))
                (progn
                  (coins-set (- (coins) 600))
                  (join (loadstr 14)))))
          (progn
            (dialog (loadstr 15))
            (defn on-dialog-closed ()
              (dialog (loadstr 16))
              (defn on-dialog-closed ()
                (alloc-space 'ladder)
                (sel-input 'ladder
                           "Place ladder (1x2):"
                           (lambda (isle x y)
                             (sound "build0")
                             (room-new (player) `(ladder ,x ,y))
                             (chr-del (opponent) 1 14)
                             (chr-new (player) x (+ 1 y) 'neutral params)
                             (dialog (loadstr 17))
                             (defn on-dialog-closed ()
                               (join (loadstr 18))
                               (setq on-dialog-closed nil)
                               (exit)))))))))
    (exit)))


(defn on-dialog-declined ()
  (exit))
