;;;
;;; neutral/1/7.lisp
;;;


(dialog "An island inhabited by an old engineer signals you. He says he's designed a new type of hull and may be able to upgrade your castle...")


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


(defn on-converge [0]
  (let ((r (filter (lambda
                     (or (equal (get $0 0) 'hull)
                         (equal (get $0 0) 'bronze-hull)))
                   (rooms (player)))))
    (let ((cost (* (length r) 160)))
      (if (length r)
          (progn
            (dialog "<c:engineer:15>Hello there! I've designed a special type of hull resistant to missiles. I'll replace all of your old hull blocks with stacked-hull, for a price of "
                    (string cost)
                    "@, you interested?")
            (dialog-await-y/n)
            (defn on-dialog-accepted [0]
              (if (< (coins) cost)
                  (progn
                    (dialog "<c:engineer:15>Sorry! you don't have enough resources, and I can't afford to upgrade your castle." (string (coins) " " cost))
                    (exit))
                (progn
                  (coins-add (* -1 cost))
                  (sound "build0")
                  (map
                   (lambda
                     (room-mut (player) (get $0 1) (get $0 2) 'stacked-hull))
                   r)
                  (adventure-log-add 37 '())
                  (dialog "<c:engineer:15> All finished! Your new hull blocks will take 75% less damage from missiles!")
                  (exit)))))
        (progn
          (dialog "<c:engineer:15>Hello there! I've designed a special type of hull resistant to missiles. I could have upgraded your hull, but you don't have any! But I'm sure we'll meet again someday!")
          (exit))
        (setq on-converge nil)))))


(defn on-dialog-declined [0]
  (dialog "<c:engineer:15>That's ok, I understand! Personally, I feel very safe from missiles with all the stacked-hull that I've built up...")
  (exit))
