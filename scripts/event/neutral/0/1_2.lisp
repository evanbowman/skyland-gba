;;;
;;; neutral/0/1_2.lisp
;;;


(dialog
 "<b:/scripts/data/img/toll_station.img.bin> "
 "An ancient imperial missile platform contacts you and demands payment...")


(opponent-init 5 'neutral)

(island-configure
 (opponent)
 '((hull 0 12)
   (arc-gun 0 10)
   (hull 0 13)
   (hull 0 8)
   (hull 0 9)
   (hull 0 7)
   (hull 0 11)
   (power-core 1 11)
   (hull 1 14)
   (hull 1 7)
   (hull 1 8)
   (power-core 1 9)
   (hull 1 13)
   (hull 2 14)
   (missile-silo 2 7)
   (hull 2 13)
   (hull 3 7)
   (hull 3 8)
   (hull 3 9)
   (hull 3 10)
   (hull 3 11)
   (hull 3 12)
   (hull 3 13)
   (rocket-bomb 4 8)
   (hull 4 11)))

(flag-show (opponent) flag-id-old-empire)


(defn on-converge ()
  (setq on-converge nil)
  (if (dialog-await-y/n
       (string "While the old empire is now fragmented and most of its weapons systems are offline, "
               "this automated vessel seems to still be functioning. <B:0>"
               "The station's computers demand a toll of 600@. Pay?"))
      (on-dialog-accepted)
      (on-dialog-declined)))


(defn/temp attack-player ((text . string))
  (opponent-mode 'hostile)
  (await (dialog* text)))


(defn on-dialog-accepted ()
  (if (< (coins) 600)
      (progn
        (adventure-log-add 59 '())
        (attack-player "Insufficient funds! The station begins charging its weapons!"))
      (progn
        (adventure-log-add 60 (list 600))
        (coins-add -600)
        (await (dialog* "The station deactivates and allows you to pass."))
        (exit))))


(defn on-dialog-declined ()
  (attack-player "The station begins charging its weapons!"))
