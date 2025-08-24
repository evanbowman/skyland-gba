;;;
;;; neutral/2/4.lisp
;;;


(dialog
 "Through sheets of rain, you spot a fortress ahead... <B:0> Its walls are wrapped in writhing mycelium, organic patterns reaching toward the high windows... <B:0>"
  "<b:/scripts/data/img/goblin_queen_close.img.bin> "
 "From her twisted throne, the goblin queen studies you intently... <B:0> Your crew warns that those who refuse her demands rarely survive to regret it...")

(weather-set weather-id-rain)

(opponent-init 9 'hostile)


(island-configure
 (opponent)
 '((hull 0 7)
   (mycelium 0 8)
   (mycelium 0 11)
   (energized-hull 0 10)
   (mycelium 0 9)
   (mycelium 0 6 40)
   (mycelium 0 14)
   (mycelium 0 12)
   (mycelium 0 13)
   (mycelium 1 12)
   (mycelium 1 14)
   (mycelium 1 6)
   (mycelium 1 7 40)
   (mycelium 1 11)
   (hull 1 13)
   (hull 1 10)
   (hull 1 8)
   (hull 1 9)
   (power-core 2 9)
   (mycelium 2 5)
   (energized-hull 2 13)
   (energized-hull 2 12)
   (mycelium 2 7)
   (energized-hull 2 14)
   (mycelium 2 6)
   (hull 2 11)
   (hull 2 8)
   (hull 3 8)
   (stairwell 3 11)
   (mycelium 3 7)
   (hull 3 6)
   (mycelium 3 5)
   (power-core 4 10)
   (mycelium 4 7)
   (stacked-hull 4 9)
   (mycelium 4 8)
   (mycelium 4 5)
   (mycelium 4 6)
   (reactor 4 12)
   (stacked-hull 5 9)
   (mycelium 5 8)
   (mycelium 5 7)
   (stairwell 6 11)
   (mycelium 6 6)
   (infirmary 6 9)
   (mycelium 6 7)
   (hull 6 8)
   (missile-silo 7 7)
   (transporter 7 13)
   (forcefield* 7 6)
   (transporter 7 11)
   (mycelium 8 8)
   (mycelium 8 7)
   (mycelium 8 6)
   (transporter 8 13)
   (transporter 8 11)
   (stacked-hull 8 10)
   (mycelium 8 9)))


(flag-show (opponent) flag-id-pirate)


(foreach
 (lambda (xy)
   (chr-new (opponent) (first xy) (second xy) 'hostile 0))
 '((6 . 14)
   (5 . 14)
   (4 . 14)
   (4 . 11)
   (2 . 10)
   (3 . 10)))



(opponent-mode 'neutral)



(let ((val (if (equal (difficulty) difficulty-beginner)
               (+ 900 (choice 500))
             (max (list (+ 900 (choice 500))
                        (/ (coins) 2))))))
  (defn on-converge ()
    (dialog
     "<c:Goblin Queen:40>#cackle# You're tressspasssing in my territory! I demand a tribute of "
     (string val)
     "@! Pay!")

    (dialog-await-binary-q-w/lore
     (format "I'll pay… (%@)" val) "No way!"
     '(("Goblin Queen?" . "It's said that she began as a promising Ashwalker Apprentice, studying ancient technology and surface lore. But where other monks sought to contain and guard dangerous artifacts, she saw opportunities for power. She began experimenting with forbidden technologies, particularly the controlled growth of mycelium... <B:0> What should we do?")))

    (setq on-converge nil))


  (defn on-dialog-accepted ()
    (if (> val (coins))
        (progn
          (opponent-mode 'hostile)
          (adventure-log-add 32 '())
          (dialog "<c:Goblin Queen:40>Thatsss not enough! Letsss sssee if theresss anything we can take!!"))
      (progn
        (coins-add (- val))
        (adventure-log-add 31 (list val))
        (dialog "The Goblin Queen rejoices, having successfully extorted "
                (string val)
                "@.")
        (exit)))))



(defn on-dialog-declined ()
  (opponent-mode 'hostile)
  (adventure-log-add 33 '())
  (dialog "<c:Goblin Queen:40>UNACCEPTABLE!! PREPARE FOR BOARDING!!!"))


(defn on-room-destroyed (isle sym)
  (if (equal isle (opponent))
      (if (equal (+ (room-count (opponent) 'reactor)
                    (room-count (opponent) 'power-core))
                 1)
          (progn
            (setq on-room-destroyed nil)
            (dialog "As victory draws near, the goblin queen slips away...")))))
