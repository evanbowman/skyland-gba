
(dialog
 "<b:/scripts/data/img/flares.img.bin>"
 "You spot strange flares arcing between the clouds... <B:0> A lone goblin perches atop a makeshift balloon, studying the patterns of island movements!")

(opponent-init -3 'neutral)

(island-configure
 (opponent)
 '((balloon 0 10)))

(terrain-set (opponent) -3)



(defn on-converge ()
  (dialog "<c:spotter:45> Sssaw your ship from my watching post! Been tracking island pathsss, marking which ones are ripe for raiding. Know all the bessst hunting grounds!")

  (setq on-dialog-closed
        (lambda ()
          (dialog "He seems harmless, invite him aboard?")

          (dialog-await-binary-q-w/lore "welcome aboard!" "sorry, but no"
                                        '(("let's chat…" .
                                           "<c:spotter:45> When rich islandsss pass overhead, I track their movement patterns. Could show you where the fat merchantsss like to hide... <B:0> Need a ssspotter on your crew?")
                                          ("explain your goggles?" .
                                           "<c:spotter:45> Ohhh! These ssspecial goggles are necesssary! <B:0> It's so bright when staring out at the cloudsss, without this visor, I'd go ssun-blind! <B:0> Anyway, I've got lotsss of experience, need a sspotter?")))

          (setq on-dialog-closed '())))
  (setq on-converge nil))


(chr-new (opponent) 1 14 'neutral '((icon . 45) (race . 1)))


(defn on-dialog-accepted ()
  (let ((temp (chr-slots (player)))
        (join (lambda (txt)
                (adventure-log-add 53 '())
                (dialog txt))))
    (if temp
        (progn
          (setq temp (get temp (choice (length temp))))
          (chr-new (player) (car temp) (cdr temp) 'neutral '((icon . 45) (race . 1)))
          (chr-del (opponent) 1 14)
          (if (or (chance 2) (< (coins) 600))
              (join "The spotter joined your crew!")
              (progn
                (coins-set (- (coins) 600))
                (join "The spotter joined your crew. Hungry, he ate 600@ of your food supplies!"))))
        (progn
          (dialog "Sadly, there's no room...")
          (defn on-dialog-closed ()
            (dialog "<c:spotter:45> No room in your castle? Hold on, I've got some supplies, I'll help out...")
            (defn on-dialog-closed ()
              (alloc-space 'ladder)
              (sel-input 'ladder
                         "Place ladder (1x2):"
                         (lambda (isle x y)
                           (sound "build0")
                           (room-new (player) `(ladder ,x ,y))
                           (chr-del (opponent) 1 14)
                           (chr-new (player) x (+ 1 y) 'neutral '((icon . 45) (race . 1)))
                           (dialog "<c:spotter:45> Thanks! I'll try to help out however I can!")
                           (defn on-dialog-closed ()
                             (join "The spotter joined your crew!")
                             (setq on-dialog-closed nil)
                             (exit)))))))))
  (exit))


(defn on-dialog-declined ()
  (exit))
