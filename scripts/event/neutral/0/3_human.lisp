;;;
;;; neutral/0/3_human.lisp
;;;


(dialog "Some merchants broadcast an advertisement for advanced technology! Let's see if they have anything useful!")

(opponent-init 5 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 0 14)))


(chr-new (opponent) 1 14 'neutral nil)
(chr-new (opponent) 2 14 'neutral nil)


(flag-show (opponent) flag-id-merchant)


(let ((item (sample '(arc-gun flak-gun fire-charge)))
      (cost 1300))

  (terrain-set (opponent) (+ (terrain (opponent)) (* 2 (car (rinfo 'size item)))))

  (map (lambda (y)
         (room-new (opponent) (list item 5 y))
         (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) y)))
       '(11 12 13 14))


  (defn on-converge ()
    (setq on-converge nil)
    (if (dialog-await-y/n (string "<c:Merchant:7>We ordered too many "
                                  (rinfo 'name item)
                                  "s and we're having a big sale today! Much cheaper than "
                                  "if you built them yourself. "
                                  cost
                                  "@ for two, what do you say?"))
        (on-dialog-accepted)
        (on-dialog-declined)))


  (setq on-dialog-declined exit)


  (defn/temp purchase-items ()
    (adventure-log-add 10 (list (rinfo 'name item) cost))
    (coins-add (- cost))
    (let ((msgs (list (string "Place first "
                              (rinfo 'name item)
                              (format " (%x%):" (car (rinfo 'size item)) (cdr (rinfo 'size item))))
                      (string "Place second " (rinfo 'name item) ":"))))
      (while msgs
        (let ((xy (await (sel-input* item (car msgs)))))
          (alloc-space item)
          (room-new (player) (list item (car xy) (cdr xy)))
          (sound "build0"))
        (setq msgs (cdr msgs))))
    (await (dialog* "<c:Merchant:7>Looks great! You made a fine choice!"))
    (exit))


  (defn/temp remove-shop ()
    (let ((xy (cdr (wg-pos))))
      ;; Switch the current map node back to visited, so that it doesn't appear
      ;; as a shop on the world map.
      (wg-node-set (first xy) (second xy) wg-id-visited)))


  (defn/temp setup-shop ()
    (let ((xy (cdr (wg-pos))))
      ;; Swap the current level type, converting it temporarily into a shop.
      (wg-node-set (first xy) (second xy) wg-id-shop)
      (await (dialog* "<c:Merchant:7>Ok! I'll be here. Come see me again when you're ready!"
                      " <B:0> (or use the START menu to return to the world map)"))))


  (defn on-dialog-accepted ()
    (if (> (coins) (decr cost))
        (purchase-items)
        (if (dialog-await-y/n (string "<c:Merchant:7>Sorry, that's not enough! "
                                      "Do you want to salvage some stuff "
                                      "to come up with the resources for payment?"))
            (setup-shop)
            (on-dialog-declined))))


  (defn on-shop-enter ()
    (if (> (coins) (decr cost))
        (if (dialog-await-y/n (string "<c:Merchant:7>Looks like you have enough now. <B:0>"
                                      "Buy two " item "s for " cost "@?"))
            (progn
              ;; In shop levels, sel-input allows you to cancel selecting
              ;; coordinates. take down the shop now that we no longer need it.
              (remove-shop)
              (purchase-items)))
        (if (not (dialog-await-binary-q (string "<c:Merchant:7>Sorry, the price was "
                                                cost "@, you're still " (- cost (coins)) "@ short…")
                                        "Salvage more stuff…"
                                        "Exit."))
            (exit))))


  (defn on-level-exit ()
    (remove-shop)))
