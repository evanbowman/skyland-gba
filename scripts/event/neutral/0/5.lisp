;;;
;;; neutral/0/5.lisp
;;;


(opponent-init 11 'neutral)

(island-configure
 (opponent)
 '((shrubbery 0 11)
   (bronze-hull 0 14)
   (bronze-hull 0 13)
   (bronze-hull 0 12)
   (masonry 1 14)
   (stairwell 1 9)
   (workshop 1 7)
   (masonry 1 13)
   (bulkhead-door 2 11)
   (masonry 2 14)
   (masonry 2 13)
   (bronze-hull 2 10)
   (bronze-hull 2 9)
   (water-source 3 14)
   (workshop 3 7)
   (bridge 3 12)
   (water-source 4 14)
   (water-source 5 14)
   (backup-core 5 11)
   (water-source 6 14)
   (water-source 7 14)
   (water-source 8 14)
   (workshop 8 11)
   (water-source 9 14)
   (bronze-hull 10 14)
   (banana-plant 10 13)))


(let ((pc (filter (lambda (equal (car $0) 'power-core)) (rooms (player))))
      (sc (filter (lambda (equal (car $0) 'backup-core)) (rooms (player)))))

  (if (or sc (not pc)) ;; player must have a core and not already have a backup
      (progn
        (defn on-converge
          (dialog "<c:mayor:11>Nice to meet ya! We were having trouble earlier, but we worked it out on our own...")
          (exit)))
    (progn
      (dialog "A small village radios you... sounds like they're having trouble with their power-core...")
      (defn on-converge

        (setq on-converge nil)

        ;; In case anything changed...
        (setq pc (filter (lambda (equal (car $0) 'power-core)) (rooms (player))))

        (dialog
         "<c:mayor:11>After a few years of use, our old power supply ran out of nuclear fuel, and we're running on this weaker standby-core. Can you help our town by trading one of your own power-cores for our standby? We'll throw in two weapons and three of our crew members to sweeten the deal!")
        (dialog-await-y/n)

        (setq on-dialog-declined exit)

        (defn on-dialog-accepted
          (let ((c (car pc)))
            (room-mut (player) (get c 1) (get c 2) 'backup-core)
            (room-mut (opponent) 5 11 'power-core)

            (let ((mkch
                   (lambda
                     (if (not (chr-slots (player)))
                         (let ((c (construction-sites (player) '(1 . 2))))
                           (if c
                               (room-new (player) `(ladder ,(car (car c)) ,(cdr (car c)))))))

                     (let ((c (chr-slots (player))))
                       (chr-new (player) (car (car c)) (cdr (car c)) 'neutral 0)))))
              (mkch)
              (mkch)
              (mkch))

            (while (< (length (construction-sites (player) '(2 . 1))) 2)
              (terrain (player) (+ (terrain (player)) 1)))

            (let ((impl
                   (lambda
                     (let ((wpn (get '(flak-gun
                                       fire-charge)
                                     (choice 2)))
                           (cb $0))

                       (sel-input '(2 . 1)
                                  (format "Place % (2x1):" wpn)
                                  (lambda
                                    (room-new (player) `(,wpn ,$1 ,$2))
                                    (cb)))))))
              (impl
               (lambda
                 (impl
                  (lambda
                    (dialog "<c:mayor:11>Thanks so much for the help!")
                    (setq on-dialog-closed exit))))))))))))
