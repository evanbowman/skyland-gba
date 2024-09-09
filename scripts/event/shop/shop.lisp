;;;
;;; shop.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")
(eval-file "/scripts/event/check_zone.lisp")


(opponent-init 6 'neutral)


(adventure-log-add 49 '())


;; The engine only knows how to process a global variable called shop-items, but
;; later, I wanted to be able to support multiple shops per zone. So we have a
;; larger list that associates map coordinates with shop items.
(if (not (lookup (wg-pos) zone-shop-items))
    (push 'zone-shop-items (cons (wg-pos)
                                 (eval-file "/scripts/event/shop/shop_items.lisp"))))

;; Bind shop items for the current world map location
(setq shop-items (lookup (wg-pos) zone-shop-items))


(defn on-shop-item-sel (nm item)
  (let ((name nm))
    (let ((info (get shop-items item)))
      (if (< (coins) (get info 1))
          (progn
            (dialog "Hah! You can't afford that!")
            (defn on-dialog-closed ()
              (push-menu "item-shop" '())))
        (progn
          (dialog
           "<c:shopkeeper:7>"
           name
           (format "? I'll sell you one for %@..." (get info 1)))

          (dialog-opts-reset)

          (dialog-opts-push
           "I'll buy it!"
           (lambda ()
             (coins-add (* -1 (get info 1)))
             (adventure-log-add 50 (list name (get info 1)))

             (alloc-space (get info 0))

             (sel-input (get info 0)
                        "pick a slot:"
                        (lambda (isle x y)
                          (room-new (player) (list (get info 0) x y))
                          (sound "build0")

                          (setq shop-items
                                (filter
                                 (lambda (item)
                                   (> (get item 2) 0))
                                 (replace shop-items
                                          (equalto? info)
                                          (list (car info)
                                                (get info 1)
                                                (- (get info 2) 1)
                                                (get info 2)))))

                          ;; Writeback the modified inner list
                          (setq zone-shop-items
                                (map (lambda (z)
                                       (if (equal (car z) (wg-pos))
                                           (cons (wg-pos) shop-items)
                                         z))
                                     zone-shop-items))

                          (if shop-items
                              (push-menu "item-shop" '())
                            (progn
                              (dialog
                               "<c:shopkeeper:7>How am I supposed to keep customers if you buy the whole store!? WE'RE CLOSED.")
                              (exit)))))))

          (dialog-opts-push (if (> (length name) 13)
                                ;; use alternate text for long block names
                                (string name " stats?")
                              (format "describe %" name))
                            (lambda ()
                              (push-menu "glossary" (list (car info)))
                              (push-menu "item-shop" '())))

          (dialog-opts-push "no thanks…"
                            (lambda ()
                              (push-menu "item-shop" '()))))))))


(defn on-fadein ()
  (dialog
   "<c:shopkeeper:7>Welcome to my shop! Let me know if you see anything you like! "
   "(when done, use the start menu to return to your sky chart)")

  (defn on-dialog-closed ()
    (push-menu "item-shop" '())))


(island-configure
 (opponent)
 '((power-core 3 13)
   (coconut-palm 5 12)
   (hull 5 14)))

(flag-show (opponent) 6)
