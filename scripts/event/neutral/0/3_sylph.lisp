;;;
;;; neutral/0/3_sylph.lisp
;;;

(dialog "A Sylph merchant vessel broadcasts a trade offer. Their equipment is known for quality...")

(opponent-init 5 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 0 14)))

(chr-new (opponent) 1 14 'neutral '((race . 4)))
(chr-new (opponent) 2 14 'neutral '((race . 4)))


(flag-show (opponent) flag-id-sylph)


(let ((item (sample '(arc-gun flak-gun fire-charge)))
      (skip 1))

  (terrain-set (opponent) (+ (terrain (opponent)) (* 2 (car (rinfo 'size item)))))

  (map (lambda (y)
         (room-new (opponent) (list item 5 y))
         (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) y)))
       '(11 12 13 14))

  (defn on-converge ()
    (dialog "<c:Sylph Merchant:46>Production optimization has resulted in surplus inventory. We're offering "
            (rinfo 'name item)
            "s at reduced cost - 1300@ for two units. Substantially below standard workshop construction costs. "
            (if (< (coins) 1300)
                "Your current funds appear insufficient. Shall I maintain position while you secure additional resources? I can wait approximately 15 seconds."
                "Do you wish to proceed with the transaction?"))
    (dialog-await-y/n)
    (setq on-converge nil))

  (defn on-dialog-accepted ()
    (if (bound? 'fut) (unbind 'fut))
    (if (< (coins) 1300)
        (progn
          ;; Capture the current executing function, reinvoke after n seconds...
          (let ((f (this)))
            (defn fut ()
              (if (> (coins) 1299)
                  (progn
                    (dialog "<c:Sylph Merchant:46>Your funds are now adequate.")
                    (setq on-dialog-closed f))
                  (f))))
          (if skip
              (progn
                (setq skip 0)
                (on-timeout 15000 'fut))
              (progn
                (dialog "<c:Sylph Merchant:46>Payment remains insufficient. Shall I wait another 15 seconds while you acquire the necessary funds?")
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
                (dialog "<c:Sylph Merchant:46>Transaction complete. The equipment should serve you well.")
                (setq on-dialog-closed exit)))))))))

(setq on-dialog-declined exit)
