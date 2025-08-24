;;;
;;; neutral/1/0_goblin.lisp
;;;


(dialog
 "An unusual structure emerges from the clouds... <B:0> A fortress of rough-hewn stone, adorned with crude but intricate carvings. <B:0> Inside, you spot goblin figures moving with unexpected discipline and precision...")


(opponent-init 5 'neutral)


(island-configure
 (opponent)
 (if (not (adv-var-load 'mercenary-event))
     '((cannon 1 13)
       (hull 2 14)
       (hull 2 13)
       (cannon 2 12)
       (power-core 3 13)
       (hull 3 12)
       (hull 4 12)
       (hull 4 11))
   (sample
    '(((cannon 1 13)
       (hull 2 14)
       (hull 2 13)
       (cannon 2 12)
       (power-core 3 13)
       (hull 3 12)
       (hull 4 12)
       (hull 4 11))
      ((hull 0 8)
       (hull 0 11)
       (cannon 0 10)
       (cannon 0 9)
       (power-core 1 9)
       (hull 1 12)
       (hull 1 11)
       (masonry 2 14 3)
       (power-core 2 11)
       (masonry 2 13 3)
       (masonry 3 14 3)
       (bronze-hull 3 13)
       (power-core 3 9)
       (hull 3 8)
       (hull 4 14)
       (hull 4 8)
       (windmill 4 7))
      ((workshop 1 9)
       (hull 1 8)
       (cannon 1 7)
       (hull 1 6)
       (windmill 2 11)
       (masonry 2 14 3)
       (power-core 2 12)
       (power-core 2 7)
       (workshop 3 10)
       (masonry 3 9 3)
       (masonry 4 9 3)
       (masonry 4 8 3)
       (shrubbery 4 7))))))

(flag-show (opponent) flag-id-marauder)


(chr-new (opponent) 0 14 'neutral '((race . 1)))
(chr-new (opponent) 1 14 'neutral '((race . 1)))


(setq on-converge
      (lambda ()
        (dialog
         "<c:Goblin Monk:41>We are the Ashwalker Order. While other goblins chase mere plunder, we seek the ancient strength that let our ancestors survive the surface world. <B:0> Our training could serve your crew well... for the right price.")

        (dialog-await-binary-q
         (format "Recruit? %@" (* 400 (zone)))
         "No thanks.")

        (setq on-converge nil)))


(defn on-dialog-accepted ()
  (if (> (* 400 (zone)) (coins))
      (progn
        (dialog "You cannot afford to pay. The monks become impatient, and cut the transmission.")
        (exit))
      (run-util-script
       "find-crew-slot"
       "<c:Goblin Monk:41>This is inconvenient, but I suppose we can help you out..."
       'ladder
       "Place block (1x2):"
       (lambda (x y _)
         (chr-del (opponent) 0 14)
         (chr-new (player) x y 'neutral '((race . 1) (icon . 41)))
         (coins-add (* -400 (zone)))
         (adventure-log-add 66 '())
         (dialog "<c:Warrior Monk:41>I live to serve!")
         (defn on-dialog-closed ()
           (setq on-dialog-closed nil)
           (dialog "The monk joined your crew!")
           (run-util-script "pickup-cart" 10
                            "The Ashwalker Monk reluctantly hands over a datacart that he'd been keeping..."
                            exit))))))



(setq on-dialog-declined
      (lambda ()
        (dialog "The monks became angry, and cut the transmission.")
        (adventure-log-add 66 '())
        (exit)))
