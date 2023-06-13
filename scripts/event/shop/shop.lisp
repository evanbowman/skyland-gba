;;;
;;; shop.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")
(eval-file "/scripts/event/check_zone.lisp")


(opponent-init 6 'neutral)


(adventure-log-add 49 '())


(if (not shop-items)
    (setq shop-items (eval-file "/scripts/event/shop/shop_items.lisp")))


(defn on-shop-item-sel
  (let ((name $0)
        (item $1))
    (let ((info (get shop-items item)))
      (if (< (coins) (get info 1))
          (progn
            (dialog "Hah! You can't afford that!")
            (defn on-dialog-closed
              (item-shop)))
        (progn
          (dialog
           "<c:shopkeeper:7>"
           name
           (format "? I'll sell you one for %@..." (get info 1)))
          (dialog-await-y/n)

          (defn on-dialog-accepted
            (coins-add (* -1 (get info 1)))
            (adventure-log-add 50 (list name (get info 1)))

            (sel-input (get info 0)
                       "pick a slot:"
                       (lambda
                         (room-new (player) (list (get info 0) $1 $2))
                         (sound "build0")

                         (setq shop-items
                               (filter
                                (lambda
                                  (> (get $0 2) 0))
                                (replace
                                 shop-items
                                 info
                                 (list (car info)
                                       (get info 1)
                                       (- (get info 2) 1)
                                       (get info 2)))))

                         (if shop-items
                             (item-shop)
                           (progn
                             (dialog
                              "<c:shopkeeper:7>How am I supposed to keep customers if you buy the whole store!? WE'RE CLOSED.")
                             (exit))))))

          (defn on-dialog-declined
            (item-shop)))))))


(defn on-fadein
  (dialog
   "<c:shopkeeper:7>Welcome to my shop! Let me know if you see anything you like! "
   "(when done, use the start menu to return to your sky chart)")

  (defn on-dialog-closed
    (item-shop)))


(island-configure
 (opponent)
 '((power-core 3 13)
   (coconut-palm 5 12)
   (hull 5 14)))

(flag-show (opponent) 6)
