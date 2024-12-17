;;;
;;; neutral/0/3.lisp
;;;


(dialog (loadstr 1))

(opponent-init 5 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 0 14)))

(let ((race (if (equal (faction) 'goblin) 1 0)))
  (chr-new (opponent) 1 14 'neutral (list (cons 'race race)))
  (chr-new (opponent) 2 14 'neutral (list (cons 'race race))))



(flag-show (opponent)
           (if (equal (faction) 'goblin)
               0
               6))


(let ((item (sample '(arc-gun flak-gun fire-charge)))
      (skip 1))

  (terrain-set (opponent) (+ (terrain (opponent)) (* 2 (car (rinfo 'size item)))))

  (map (lambda (y)
         (room-new (opponent) (list item 5 y))
         (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) y)))
       '(11 12 13 14))

  (setq on-converge
        (lambda ()
          (dialog (loadstr 2)
                  (rinfo 'name item)
                  (loadstr 3)
                  (if (< (coins) 1300)
                      (loadstr 4)
                      (loadstr 5)))
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
                          (dialog (loadstr 6))
                          (setq on-dialog-closed f))
                        (f))))

                (if skip
                    (progn
                      (setq skip 0)
                      (on-timeout 15000 'fut))
                    (progn
                      (dialog (loadstr 7))
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
                  "place first "
                  (rinfo 'name item)
                  (format " (%x%):" (car (rinfo 'size item)) (cdr (rinfo 'size item))))
                 (lambda (isle x y)
                   (room-new (player) (list item x y))
                   (sound "build0")
                   (alloc-space item)
                   (sel-input
                    item
                    (string "place second " (rinfo 'name item) ":")
                    (lambda (isle x y)
                      (room-new (player) (list item x y))
                      (sound "build0")
                      (dialog (loadstr 8))
                      (setq on-dialog-closed exit))))))))))

(setq on-dialog-declined exit)
