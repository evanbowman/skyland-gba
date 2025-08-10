;;;
;;; neutral/1/6_1.lisp
;;;


(dialog
"<b:/scripts/data/img/engineer.img.bin>"
 "On a nearby island, an old engineer signals you from his workshop. He says he has designed something new and is excited to share it with someone!")


(opponent-init 7 'neutral)

(island-configure
 (opponent)
 '((stacked-hull 0 14)
   (manufactory 1 10)
   (stacked-hull 1 9)
   (stacked-hull 2 9)
   (stacked-hull 2 14)
   (stacked-hull 3 14)
   (stacked-hull 3 12)
   (stacked-hull 3 13)
   (stacked-hull 3 9)
   (stacked-hull 4 11)
   (stacked-hull 4 12)
   (stacked-hull 4 9)
   (stacked-hull 4 10)
   (power-core 4 13)
   (stacked-hull 5 12)
   (stacked-hull 6 14)
   (stacked-hull 6 12)
   (stacked-hull 6 13)
   (stacked-hull 6 10)
   (stacked-hull 6 11)))


(defn on-converge ()
  (let ((r (filter (lambda (room)
                     (or (equal (get room 0) 'hull)
                         (equal (get room 0) 'bronze-hull)))
                   (rooms (player)))))
    (let ((cost (* (length r) 160)))
      (if (length r)
          (progn
            (dialog "<c:Engineer:15>Hello there! I've designed a special type of hull resistant to missiles. I'll replace all of your old hull blocks with stacked-hull, for a price of "
                    (string cost)
                    "@, you interested?")
            (dialog-await-y/n)
            (defn on-dialog-accepted ()
              (if (< (coins) cost)
                  (progn
                    (dialog "<c:Engineer:15>Sorry! You don't have enough resources, and I can't afford to upgrade your castle." (string (coins) " " cost))
                    (exit))
                (progn
                  (coins-add (* -1 cost))
                  (sound "build0")
                  (map
                   (lambda (room)
                     (room-mut (player) (get room 1) (get room 2) 'stacked-hull))
                   r)
                  (adventure-log-add 37 '())
                  (dialog "<c:Engineer:15> All finished! Your new hull blocks will take 75% less damage from missiles!")
                  (exit)))))
        (progn
          (dialog "<c:Engineer:15>Hello there! I've designed a special type of hull resistant to missiles. I could have upgraded your hull, but you don't have any! But I'm sure we'll meet again someday!")
          (exit)))))
  (setq on-converge nil))


(defn on-dialog-declined ()
  (dialog "<c:Engineer:15>That's OK, I understand! Personally, I feel very safe from missiles with all the stacked-hull that I've built up...")
  (exit))
