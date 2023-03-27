;;;
;;; neutral/2/5.lisp
;;;


(dialog "A shop approaches! Looks like they have a discount on an advanced type of hull...")


(opponent-init 8 'neutral)

(island-configure
 (opponent)
 '((shrubbery 0 13)
   (masonry 0 14)
   (shrubbery 1 14)
   (mirror-hull 2 11)
   (mirror-hull 2 12)
   (mirror-hull 2 13)
   (mirror-hull 2 14)
   (mirror-hull 3 12)
   (mirror-hull 3 14)
   (mirror-hull 3 13)
   (mirror-hull 3 11)
   (water-source 4 14)
   (power-core 5 13)
   (hull 6 12)
   (masonry 7 14)
   (masonry 7 13)
   (hull 7 12)))


(chr-new (opponent) 6 11 'neutral 0)


(setq on-dialog-declined
      (lambda
        (if (bound 'fut)
            (unbind 'fut))
        (exit)))


(defn fut
  (if (bound 'fut) (unbind 'fut))

  (if (< (coins) 2000)
      (progn
        (dialog "<c:merchant:7> Sorry, that's not enough! Want a chance to salvage some stuff to come up with the funds? I'll check back in 15 seconds?")
        (dialog-await-y/n)
        (let ((f (this)))
          (setq fut
                (lambda
                  (if (> (coins 1999))
                      (progn
                        (dialog "<c:merchant:7> Seems like you have enough now!")
                        (setq on-dialog-closed f))
                    (f)))))
        (setq on-dialog-accepted (lambda (on-timeout 15000 'fut)))
        (setq on-dialog-declined (lambda (unbind 'fut) (exit))))
    (progn
      ;; If there's no place to put a block, grant the player +1 terrain.
      (if (not (construction-sites (player) '(1 . 1)))
          (terrain (player) (+ (terrain (player)) 1)))

      (coins-add -2000)

      (let ((cnt 0)) ; closure
        ;; lambda invoked 8 times, ask user for input and create a block at
        ;; position.
        ((lambda
           (let ((t (this))
                 (del 0))
             (sel-input
              'mirror-hull
              (string "place block " (+ cnt 1) "/8:")
              (lambda
                (room-new (player) (list 'mirror-hull $1 $2))
                (syscall "sound" "build0")
                (+= cnt 1)

                ;; delete a block from the shop's inventory
                (map (lambda
                       (if (and (equal (car $0) 'mirror-hull)
                                (not del))
                           (progn
                             (setq del 1)
                             (room-del (opponent) (get $0 1) (get $0 2)))))

                     (rooms (opponent)))

                (if (equal cnt 8)
                    (progn
                      (dialog "<c:merchant:7> Good luck to ya!")
                      (setq on-dialog-closed nil)
                      (exit))
                  (t)))))))))))


(defn on-converge
  (dialog
   "<c:merchant:7> We're selling mirror-hull, and at a steep discount! 2000@ for eight,"
   (if (< (coins) 2000)
       " ...but you don't seem to have enough. Want a chance to salvage some stuff to come up with the funds? I'll check back in 15 seconds?"
      " whaddya say?"))

  (setq on-dialog-accepted
        (if (< (coins) 2000)
            (let ((f fut))
              (setq fut
                    (lambda
                      (if (> (coins) 1999)
                          (progn
                            (dialog "<c:merchant:7> Seems like you have enough now!")
                            (setq on-dialog-closed f))
                        (f))))
              (lambda (on-timeout 15000 'fut)))
          fut))

  (dialog-await-y/n)
  ;; We don't want on-converge to be re-invoked, we add terrain in some cases.
  (setq on-converge nil))
