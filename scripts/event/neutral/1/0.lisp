;;;
;;; neutral/1/0.lisp
;;;


(dialog
 ;; The first time this event happens, show a cover image
 (if (not (adv-var-load "mercenary-event"))
     "<b:/scripts/data/img/marauder.img.bin> "
   "")
 "You discover a fortress inhabited by some mercenaries...")


(opponent-init 5 'neutral)


(island-configure
 (opponent)
 (if (not (adv-var-load "mercenary-event"))
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

(flag-show (opponent) 1)


(chr-new (opponent) 0 14 'neutral 0)
(chr-new (opponent) 1 14 'neutral 0)


(setq on-converge
      (lambda ()
        (dialog "One of the mercenaries offers to join you crew, for a cost of "
                (string (* 400 (zone)))
                "@. Accept offer?")

        (dialog-await-y/n)
        (setq on-converge nil)))


(defn on-dialog-accepted ()
  (let ((dest (chr-slots (player))))
    (if (> (* 400 (zone)) (coins))
        (progn
          (dialog "You cannot afford to pay. The mercenaries become impatient, and cut the transmission.")
          (exit))
      (if dest
          (progn
            (coins-add (* -400 (zone)))
            (setq dest (sample dest))
            (chr-new (player) (car dest) (cdr dest) 'neutral nil)
            (chr-del (opponent) 0 14)
            (dialog "<c:mercenary:17> Ahoy! Ready to knock some heads!?")
            (defn on-dialog-closed ()
              (setq on-dialog-closed nil)
              (dialog "The mercenary joined your crew!")
              (exit))
            (adventure-log-add 27 (list (* 400 (zone)))))
        (progn
          (dialog "Sadly, there's no room...")
          (exit))))))



(setq on-dialog-declined
      (lambda ()
        (dialog "The mercenaries became angry, and cut the transmission.")
        (adventure-log-add 28 '())
        (exit)))


(adv-var-store "mercenary-event" true)
