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


(flag-show (opponent) flag-id-colonist)


(defn/temp power-supplies ()
  (let ((rlat (rooms (player))))
    (list (cons 'cores (append (filter (car-equalto? 'power-core) rlat)
                               (filter (car-equalto? 'overdrive-core) rlat)))
          (cons 'reactors (append (filter (car-equalto? 'reactor) rlat)
                                  (filter (car-equalto? 'chaos-core) rlat)))
          (cons 'backups (filter (car-equalto? 'backup-core) rlat)))))



(let ((info (power-supplies))
      (wpn (sample '(flak-gun fire-charge ballista))))

    (when (lookup 'cores info)
      (secret 1 14 (string "Notice: Surplus " wpn " in stock!")))

    (if (or (lookup 'backups info) (and (not (lookup 'reactors info))
                                        (not (lookup 'cores info)))) ;; Player must have a core and not already have a backup.
        (defn on-converge ()
          (await (dialog* "<c:Mayor:10>Nice to meet ya! We were having trouble earlier, "
                          "but we worked it out on our own..."))
          (exit))
        (progn
          (dialog "A small village radios you... "
                  "sounds like they're having trouble with their power-core...")

          (defn on-converge ()
            (setq on-converge nil)

            ;; In case anything changed...
            (setq info (power-supplies))

            (if (dialog-await-binary-q (string "<c:Mayor:10>After a few years of use, our old power "
                                               "supply ran out of atomic fuel, and we're running on "
                                               "this weaker standby-core. Can you help our town by "
                                               "trading one of your own power-cores for our "
                                               "standby? We'll throw in two weapons and three of our "
                                               "crew members to sweeten the deal!")
                                       "OK, let's trade!"
                                       "Sorry, I can't…")
                (on-dialog-accepted)
                (on-dialog-declined)))))


    (setq on-dialog-declined exit)


    (defn/temp add-weapon ()
      (alloc-space wpn)
      (let ((xy (await (sel-input* wpn (string "Place " (rinfo 'name wpn)
                                               (format " (%x%):"
                                                       (car (rinfo 'size wpn))
                                                       (cdr (rinfo 'size wpn))))))))
        (room-new (player) `(,wpn ,(car xy) ,(cdr xy)))
        (sound "build0")))


    (defn/temp add-crewmember ()
      (if (not (chr-slots (player)))
          (let ((c (construction-sites (player) '(1 . 2))))
            (if c
                (room-new (player) `(ladder ,(caar c) ,(cdr (car c)))))))

      (let ((slots (chr-slots (player))))
        (chr-new (player)
                 (caar slots)
                 (cdr (car slots))
                 'neutral
                 nil)))


    (defn on-dialog-accepted ()
      (let ((del nil))
        (if (lookup 'cores info)
            (setq del (car (lookup 'cores info)))
            (progn
              ;; The player has no power-core, but is instead donating a
              ;; reactor. Give a potentially rare weapon!
              (setq del (car (lookup 'reactors info)))
              (setq wpn (sample '(ballista
                                  annihilator
                                  decimator
                                  rocket-bomb
                                  warhead
                                  particle-lance
                                  incinerator)))))

        (room-mut (player) (get del 1) (get del 2) 'backup-core)
        (room-mut (opponent) 5 11 'power-core)

        (foreach-async add-crewmember (range 3))
        (foreach-async add-weapon (range 2))

        (adventure-log-add 36 (list (rinfo 'name wpn) 3))

        (pickup-cart 3 "<c:Mayor:10>Oh, I almost forgot! When removing the old core, we found some documents left by a mechanic from the last time we replaced a core. <B:0> We have no use for these records, why don't you take them!")
        (exit))))
