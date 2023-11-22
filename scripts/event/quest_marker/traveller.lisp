

(dialog "You reach the location that the mysterious man labeled on your map...")


(defn on-fadein
  (map (lambda
         (if (equal (get $0 0) 'torch)
             (fire-new (opponent) (get $0 1) (get $0 2))))
       (rooms (opponent)))
  (setq on-fadein nil))


(let ((id nil))
  (map (lambda
         (let ((icon (assoc 'icon (cdr (cdr $0)))))
           (if (and icon (equal (cdr icon) 23))
               (setq id (cdr (assoc 'id (cdr (cdr $0))))))))
       (chrs (player)))

  (defn on-converge
    (setq on-converge nil)
    (if tr
        (progn

          (let ((tab (eval-file "/scripts/config/room_tab.lisp"))
                (sel '())
                (eq (lambda
                      (let ((sym $0)
                            (ret 0))
                        (map (lambda
                               (if (equal $0 sym)
                                   (setq ret 1)))
                             $1)
                        ret))))

            (while (< (length sel) 1)
              (let ((pick (sample tab)))
                (let ((cost (get pick 2)))
                  (when (> cost 1000)
                    (setq sel (cons (car pick) sel))))))

            (dialog "<c:traveller:23> We've arrived! Unfortunately, it's time we parted ways; I need to start repairs and move my island out of the way of this storm. I'm grateful for your help, here're a few things that you may find useful (2000@ and one random block) ")
            (defn on-dialog-closed
              (coins-add 2000)
              (let ((sym0 (get sel 0)))
                (while (not (construction-sites (player) (rinfo 'size sym0)))
                  (terrain (player) (+ (terrain (player)) 1)))
                (sel-input sym0
                           (string "Place " (rinfo 'name sym0))
                           (lambda
                             (sound "build0")
                             (room-new (player) (list sym0 $1 $2))
                             (dialog "<c:traveller:23> Goodbye, and good luck!")
                             (map (lambda
                                    (if (equal id (cdr (assoc 'id (cdr (cdr $0)))))
                                        (chr-del (player) (car $0) (car (cdr $0)))))
                                  (chrs (player)))
                             (defn on-dialog-closed
                               (dialog "The traveller returned to his island!")
                               (setq on-dialog-closed exit))))))))
      (progn
        (dialog "Unfortunately, it seems the traveller is no longer aboard your island...")
        (exit)))))


(opponent-init 7 'neutral)

(island-configure
 (opponent)
 '((lemon-tree 0 13)
   (workshop 1 11)
   (masonry 1 14 0)
   (masonry 2 13 3)
   (masonry 2 14 3)
   (power-core 3 11)
   (masonry 3 14 3)
   (masonry 3 13 3)
   (torch 4 9)
   (masonry 4 13 3)
   (masonry 4 14 3)
   (masonry 5 14 0)
   (masonry 5 12 0)
   (masonry 5 11 0)
   (windmill 5 13)
   (masonry 6 14 2)
   (masonry 6 13 0)
   (masonry 6 12 0)
   (torch 6 11)
   (plundered-room 6 8)))
