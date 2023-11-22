;;;
;;; neutral/0/3.lisp
;;;


(dialog "Some merchants broadcast an advertisement for advanced technology! Let's see if they have anything useful!")


(opponent-init 5 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (hull 0 14)))

(chr-new (opponent) 1 14 'neutral 0)
(chr-new (opponent) 2 14 'neutral 0)


(flag-show (opponent) 6)


(let ((mktr (lambda
              ;; Generate some terrain! Just to avoid the situation where
              ;; there's not enough space on the island to actually place the
              ;; weapon that we just bought. The game would basically soft-lock.
              (while (not (construction-sites (player) $0))
                ;; Give the player +1 terrain until a construction site exists.
                (terrain (player) (+ (terrain (player)) 1)))))

      (item (sample '(arc-gun flak-gun fire-charge)))
      (skip 1))

  (terrain (opponent) (+ (terrain (opponent)) (* 2 (car (rinfo 'size item)))))
  (room-new (opponent) (list item 5 14))
  (room-new (opponent) (list item 5 13))
  (room-new (opponent) (list item 5 12))
  (room-new (opponent) (list item 5 11))

  (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) 14))
  (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) 13))
  (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) 12))
  (room-new (opponent) (list item (+ 5 (car (rinfo 'size item))) 11))

  (setq on-converge
        (lambda
          (dialog "<c:merchant:7> We ordered too many "
                  (rinfo 'name item)
                  "s and we're having a big sale today! Much cheaper than if you built them yourself. 1300@ for two, "
                  (if (< (coins) 1300)
                      "...but you don't seem to have enough. Do you want to salvage some stuff to come up with the funds? I'll check back in 15 seconds?"
                    "what do you say?"))
          (dialog-await-y/n)
          (setq on-converge nil)))



  (setq on-dialog-accepted
        (lambda
          (if (bound 'fut) (unbind 'fut))

          (if (< (coins) 1300)
              (progn
                ;; Capture the current executing function, reinvoke after n seconds...
                (let ((f (this)))
                  (setq fut
                        (lambda
                          (if (> (coins) 1299)
                              (progn
                                (dialog "<c:merchant:7> Seems like you have enough now!")
                                (setq on-dialog-closed f))
                            (f)))))

                (if skip
                    (progn
                      (setq skip 0)
                      (on-timeout 15000 'fut))
                  (progn
                    (dialog "<c:merchant:7> Sorry, that's not enough money! Do you want to salvage some stuff to come up with the funds? I'll check back in in 15 seconds?")
                    (dialog-await-y/n)
                    (setq on-dialog-accepted (lambda (on-timeout 15000 'fut)))
                    (setq on-dialog-declined (lambda (unbind 'fut) (exit))))))
            (progn
              (adventure-log-add 10 (list (rinfo 'name item) 1300))
              (coins-add -1300)
              (mktr (rinfo 'size item))
              (sel-input
               item
               (string
                "place first "
                (rinfo 'name item)
                (format " (%x%):" (car (rinfo 'size item)) (cdr (rinfo 'size item))))
               (lambda
                 (room-new (player) (list item $1 $2))
                 (sound "build0")
                 (mktr (rinfo 'size item))
                 (sel-input
                  item
                  (string "place second " (rinfo 'name item) ":")
                  (lambda
                    (room-new (player) (list item $1 $2))
                    (sound "build0")
                    (dialog "<c:merchant:7> Looks great! You made a fine choice!")
                    (setq on-dialog-closed exit))))))))))

(gc) ;; just in case, no harm in running it.



(setq on-dialog-declined
      (lambda
        (exit)))
