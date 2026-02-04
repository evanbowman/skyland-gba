;;;
;;; neutral/0/3_sylph.lisp
;;;


(dialog "A Sylph supply depot appears through the clouds, its crystalline sensors tracking your approach. <B:0> Your radio crackles with a precise, emotionless transmission...")

(opponent-init 5 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 0 14)))



(chr-new (opponent) 1 14 'neutral '((race . 4)))
(chr-new (opponent) 2 14 'neutral '((race . 4)))


(flag-show (opponent) flag-id-sylph) ;; Sylph flag


(let ((item (sample '(arc-gun flak-gun fire-charge)))
      (cost 1300))

  (terrain-set (opponent) (+ (terrain (opponent)) (* 2 (car (rinfo 'size item)))))

  (map (lambda (y)
         (room-new (opponent) (list item 5 y))
         (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) y)))
       '(11 12 13 14))


  (defn on-converge ()
    (setq on-converge nil)
    (if (dialog-await-binary-q (string "<c:Sylph Quartermaster:53>Scanning vessel configuration... <B:0> Analysis: sub-optimal armament for projected threat parameters in your operational zone. <B:0> Conclave logistics has authorized requisition of two "
                                       (rinfo 'name item)
                                       " emplacements to your vessel. <B:0> Cost: materials only, "
                                       cost
                                       "@.")
                               (string "Accept for " cost "@.")
                               "Refuse allocation.")
        (on-dialog-accepted)
        (on-dialog-declined)))


  (defn on-dialog-declined ()
    (await (dialog* "<c:Sylph Quartermaster:53>Acknowledged. Tactical assessment indicates 73% probability you will not survive the next three hostile encounters with current armament. <B:0> Your decision is... inefficient. <B:0> Requisition cancelled."))
    (exit))


  (defn/temp purchase-items ()
    (adventure-log-add 10 (list (rinfo 'name item) cost))
    (coins-add (- cost))
    (let ((msgs (list (string "Install first "
                              (rinfo 'name item)
                              (format " (%x%):" (car (rinfo 'size item)) (cdr (rinfo 'size item))))
                      (string "Install second " (rinfo 'name item) ":"))))
      (while msgs
        (let ((xy (await (sel-input* item (car msgs)))))
          (alloc-space item)
          (room-new (player) (list item (car xy) (cdr xy)))
          (sound "build0"))
        (setq msgs (cdr msgs))))
    (await (dialog* "<c:Sylph Quartermaster:53>Transfer complete. Conclave tactical projections indicate improved survival probability. <B:0> Maintain operational readiness. Quartermaster out."))
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
      (await (dialog* "<c:Sylph Quartermaster:53>Standing by. Come see me again when you're ready!"
                      " <B:0> (or use the START menu to return to the world map)"))))


  (defn on-dialog-accepted ()
    (if (> (coins) (decr cost))
        (purchase-items)
        (if (dialog-await-y/n (string "<c:Sylph Quartermaster:53>Sorry, that's not enough! "
                                      "Do you want to salvage some stuff "
                                      "to come up with the resources for payment?"))
            (setup-shop)
            (on-dialog-declined))))


  (defn on-shop-enter ()
    (if (> (coins) (decr cost))
        (if (dialog-await-y/n (string "<c:Sylph Quartermaster:53>Looks like you have enough now. "
                                      "<B:0>"
                                      "Accept two " item "s for " cost "@?"))
            (progn
              ;; In shop levels, sel-input allows you to cancel selecting
              ;; coordinates. take down the shop now that we no longer need it.
              (remove-shop)
              (purchase-items)))
        (if (not (dialog-await-binary-q (string "<c:Sylph Quartermaster:53>Sorry, the cost was "
                                                cost "@, you're still " (- cost (coins)) "@ short…")
                                        "Salvage more stuff…"
                                        "Exit."))
            (exit))))


  (defn on-level-exit ()
    (remove-shop)))
