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
      (skip 1))

  (terrain-set (opponent) (+ (terrain (opponent)) (* 2 (car (rinfo 'size item)))))

  (map (lambda (y)
         (room-new (opponent) (list item 5 y))
         (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) y)))
       '(11 12 13 14))

  (setq on-converge
        (lambda ()
          (dialog "<c:Merchant:7>We ordered too many "
                  (rinfo 'name item)
                  "s and we're having a big sale today! Much cheaper than if you built them yourself. 1300@ for two, "
                  (if (< (coins) 1300)
                      "...but you don't seem to have enough. Do you want to salvage some stuff to come up with the funds? I'll check back in 15 seconds?"
                      "What do you say?"))
          (dialog-await-y/n)
          (setq on-converge nil)))



  (setq on-dialog-accepted
        (lambda ()
          (if (bound? 'fut) (unbind 'fut))

          (if (< (coins) 1300)
              (progn
                ;; Capture the current executing function, reinvoke after n seconds...
                (let ((f (this)))
                  (defn fut ()
                    (if (> (coins) 1299)
                        (progn
                          (dialog "<c:Merchant:7>Seems like you have enough now!")
                          (setq on-dialog-closed f))
                        (f))))

                (if skip
                    (progn
                      (setq skip 0)
                      (on-timeout 15000 'fut))
                    (progn
                      (dialog "<c:Merchant:7>Sorry, that's not enough! Do you want to salvage some stuff to come up with the resources for payment? I'll check back in in 15 seconds?")
                      (dialog-await-y/n)
                      (setq on-dialog-accepted (lambda () (on-timeout 15000 'fut)))
                      (setq on-dialog-declined (lambda () (unbind 'fut) (exit))))))
              (progn
                (adventure-log-add 10 (list (rinfo 'name item) 1300))
                (coins-add -1300)
                (alloc-space item)
                (sel-input
                 item
                 (string
                  "Place first "
                  (rinfo 'name item)
                  (format " (%x%):" (car (rinfo 'size item)) (cdr (rinfo 'size item))))
                 (lambda (isle x y)
                   (room-new (player) (list item x y))
                   (sound "build0")
                   (alloc-space item)
                   (sel-input
                    item
                    (string "Place second " (rinfo 'name item) ":")
                    (lambda (isle x y)
                      (room-new (player) (list item x y))
                      (sound "build0")
                      (dialog "<c:Merchant:7>Looks great! You made a fine choice!")
                      (setq on-dialog-closed exit))))))))))

(setq on-dialog-declined exit)
