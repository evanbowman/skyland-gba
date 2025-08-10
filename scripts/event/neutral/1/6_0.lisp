;;;
;;; neutral/1/6_0.lisp
;;;


(opponent-init 11 'neutral)

(island-configure
 (opponent)
 '((windmill 0 9)
   (bronze-hull 0 13)
   (bronze-hull 0 12)
   (bronze-hull 0 14)
   (shrubbery 0 11)
   (hull 1 14)
   (workshop 1 7)
   (stairwell 1 9)
   (masonry 1 13 0)
   (bronze-hull 2 10)
   (masonry 2 14 0)
   (masonry 2 13 0)
   (bronze-hull 2 9)
   (bulkhead-door 2 11)
   (water-source 3 14)
   (bridge 3 12)
   (workshop 3 7)
   (water-source 4 14)
   (backup-core 5 11)
   (water-source 5 14)
   (sunflower 6 8)
   (masonry 6 9 3)
   (water-source 6 14)
   (masonry 6 10 3)
   (masonry 7 10 3)
   (lemon-tree 7 7)
   (masonry 7 12 3)
   (masonry 7 11 3)
   (masonry 7 9 3)
   (water-source 7 14)
   (water-source 8 14)
   (workshop 8 11)
   (masonry 8 10 3)
   (masonry 8 9 3)
   (workshop 8 7)
   (water-source 9 14)
   (bronze-hull 10 14)
   (banana-plant 10 13)))


(flag-show (opponent) 7)


(let ((pc (filter (car-equalto? 'power-core)  (rooms (player))))
      (rc (filter (car-equalto? 'reactor)     (rooms (player))))
      (sc (filter (car-equalto? 'backup-core) (rooms (player))))
      (wpn (sample '(flak-gun fire-charge ballista))))

    (if (not pc)
        (setq pc (filter (car-equalto? 'overdrive-core) (rooms (player)))))

    (when pc
      (secret 1 14 (string "Notice: Surplus " wpn " in stock!")))

    (if (or sc (and (not rc) (not pc))) ;; Player must have a core and not already have a backup.
        (progn
          (defn on-converge ()
            (dialog "<c:Mayor:10>Nice to meet ya! We were having trouble earlier, but we worked it out on our own...")
            (exit)))
        (progn
          (dialog "A small village radios you... sounds like they're having trouble with their power-core...")
          (defn on-converge ()

            (setq on-converge nil)

            ;; In case anything changed...
            (setq pc (filter (car-equalto? 'power-core) (rooms (player))))
            (setq rc (filter (car-equalto? 'reactor) (rooms (player))))

            (dialog
             "<c:Mayor:10>After a few years of use, our old power supply ran out of atomic fuel, and we're running on this weaker standby-core. Can you help our town by trading one of your own power-cores for our standby? We'll throw in two weapons and three of our crew members to sweeten the deal!")
            (dialog-await-binary-q "OK, let's trade!" "Sorry, I can't…")

            (setq on-dialog-declined exit)

            (defn on-dialog-accepted ()
              (let ((c nil))
                (if pc
                    (setq c (car pc))
                    (progn
                      ;; The player has no power-core, but is instead donating a
                      ;; reactor. Give a potentially rare weapon!
                      (setq c (car rc))
                      (setq wpn (sample '(ballista
                                          annihilator
                                          decimator
                                          rocket-bomb
                                          warhead
                                          incinerator)))))

                (room-mut (player) (get c 1) (get c 2) 'backup-core)
                (room-mut (opponent) 5 11 'power-core)

                (let ((mkch
                       (lambda ()
                        (if (not (chr-slots (player)))
                            (let ((c (construction-sites (player) '(1 . 2))))
                              (if c
                                  (room-new (player) `(ladder ,(caar c) ,(cdr (car c)))))))

                        (let ((c (chr-slots (player))))
                          (chr-new (player)
                                   (caar c)
                                   (cdr (car c))
                                   'neutral
                                   nil)))))
                  (mkch)
                  (mkch)
                  (mkch))

                (alloc-space wpn)

                (adventure-log-add 36 (list (rinfo 'name wpn) 3))

                (let ((impl
                       (lambda (cb)
                        (let ((resp cb))

                          (sel-input wpn
                                     (string "Place " (rinfo 'name wpn)
                                             (format " (%x%):"
                                                     (car (rinfo 'size wpn))
                                                     (cdr (rinfo 'size wpn))))
                                     (lambda (isle x y)
                                      (room-new (player) `(,wpn ,x ,y))
                                      (sound "build0")
                                      (resp)))))))
                  (impl
                   (lambda ()
                    (impl
                     (lambda ()
                      (dialog "<c:Mayor:10>Thanks so much for the help!")
                      (run-util-script "pickup-cart" 3
                                       "<c:Mayor:10>Oh, I almost forgot! When removing the old core, we found some documents left by a mechanic from the last time we replaced a core. <B:0> We have no use for these records, why don't you take them!"
                                       exit))))))))))))
