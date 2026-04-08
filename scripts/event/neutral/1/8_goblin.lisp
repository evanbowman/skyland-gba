;;;
;;; neutral/1/8_goblin.lisp
;;;

(tr-bind-current)


(dialog
 "<b:/scripts/data/img/flares.img.bin>"
 (tr "You spot strange flares arcing between the clouds... <B:0> A lone goblin perches atop a makeshift balloon, studying the patterns of island movements!"))

(opponent-init -3 'neutral)

(island-configure
 (opponent)
 '((balloon 0 10)))

(terrain-set (opponent) -3)



(defn on-converge ()
  (setq on-converge nil)
  (await (dialog* (tr "<c:Spotter:45>Sssaw your ship from my watching post! Been tracking island pathsss, marking which ones are ripe for raiding. Know all the bessst hunting groundsss!")))

  (if (dialog-await-binary-q-w/lore (tr "He seems harmless, invite him aboard?")
                                    (tr "Welcome aboard!")
                                    (tr "Sorry, but no.")
                                    (tr '(("Let's chat…" .
                                           "<c:Spotter:45>When rich islandsss passs overhead, I track their movement patterns. Could show you where the fat merchantsss like to hide... <B:0> Need a ssspotter on your crew?")
                                          ("Explain your goggles?" .
                                           "<c:Spotter:45>Ohhh! These ssspecial goggles are necesssary! <B:0> It's so bright when staring out at the cloudsss, without this visor, I'd go sssun-blind! <B:0> Anyway, I've got lotsss of experience, need a ssspotter?"))))
      (on-dialog-accepted)
      (on-dialog-declined)))


(chr-new (opponent) 1 14 'neutral '((icon . 45) (race . 1)))


(defn/temp find-goblin-crewmember ()
;  (breakpoint)
  (let ((choices (filter (lambda (chr)
                           (let* ((plist (cddr chr))
                                  (icon (lookup 'icon plist)))
                             (and icon
                                  (not (equal icon 45))
                                  (equal 1 (lookup 'race plist)))))
                         (chrs (player)))))
    (if choices
        (sample choices)
        '(nil nil (race . 1) (icon . 2)))))


(defn/temp join-crew (message)
  (adventure-log-add 53 '())
  (await (dialog* message))
  (when (not (cart-found? 13))
    (let ((speaker (find-goblin-crewmember)))
      (pickup-cart 13
                   (format (tr "<c:Crew:%> Captain! Thisss one hasss been hiding sssomething! Found thisss cart sssstashed in his bunk. Could be sssecret codes, maybe he'sss a double agent! It saysss 'do NOT read'!!")
                           (lookup 'icon (cddr speaker))))
      (await (dialog* (tr "<c:Spotter:45> That wasss PRIVATE!")))
      (await (dialog* (tr "(The spotter hisses something unprintable and climbs to his lookout post, although one of your crew thinks he saw him carrying a pen and a few sheets of paper...)"))))))


(defn on-dialog-accepted ()
  (let ((temp (chr-slots (player)))
        (join (lambda (txt)

                (dialog txt))))
    (if temp
        (progn
          (setq temp (get temp (choice (length temp))))
          (chr-new (player) (car temp) (cdr temp) 'neutral '((icon . 45) (race . 1)))
          (chr-del (opponent) 1 14)
          (if (or (chance 2) (< (coins) 600))
              (join-crew (tr "The spotter joined your crew!"))
              (progn
                (coins-set (- (coins) 600))
                (join-crew (tr "The spotter joined your crew. Hungry, he ate 600@ of your food supplies!")))))
        (progn
          (dialog (tr "Sadly, there's no room..."))
          (defn on-dialog-closed ()
            (dialog (tr "<c:Spotter:45>No room in your castle? Hold on, I've got some supplies, I'll help out..."))
            (defn on-dialog-closed ()
              (alloc-space 'ladder)
              (sel-input 'ladder
                         (tr "Place ladder (1x2):")
                         (lambda (isle x y)
                           (sound "build0")
                           (room-new (player) `(ladder ,x ,y))
                           (chr-del (opponent) 1 14)
                           (chr-new (player) x (+ 1 y) 'neutral '((icon . 45) (race . 1)))
                           (dialog (tr "<c:Spotter:45>Thanks! I'll try to help out however I can!"))
                           (defn on-dialog-closed ()
                             (setq on-dialog-closed nil)
                             (join-crew (tr "The spotter joined your crew!"))
                             (exit)))))))))
  (exit))


(defn on-dialog-declined ()
  (exit))
