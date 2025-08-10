;;;
;;; quest_marker/traveller.lisp
;;;


(dialog "You reach the location that the mysterious man labeled on your map...")


(defn on-fadein ()
  (map (lambda (room)
         (if (equal (get room 0) 'torch)
             (fire-new (opponent) (get room 1) (get room 2))))
       (rooms (opponent)))
  (setq on-fadein nil))


(let ((traveller-id (lookup 5 qvar))
      (id nil))
  (foreach (lambda (chr)
             (if (equal traveller-id (lookup 'id (cddr chr)))
                 (setq id (lookup 'id (cddr chr)))))
           (chrs (player)))

  (defn on-converge ()
    (setq on-converge nil)
    (if id
        (progn
          (let ((tab (eval-file "/scripts/config/room_tab.lisp"))
                (sel '()))

            (while (< (length sel) 1)
              (let ((pick (sample tab)))
                (let ((cost (get pick 2)))
                  (when (> cost 1000)
                    (setq sel (cons (car pick) sel))))))

            (dialog "<c:Traveller:23>We've arrived! Unfortunately, it's time we parted ways; I need to start repairs and move my island out of the way of this storm. I'm grateful for your help, here're a few things that you may find useful. (2000@ and one random block) ")
            (defn on-dialog-closed ()
              (coins-add 2000)
              (let ((sym0 (get sel 0)))
                (alloc-space sym0)
                (sel-input sym0
                           (string "Place " (rinfo 'name sym0))
                           (lambda (isle x y)
                             (sound "build0")
                             (room-new (player) (list sym0 x y))
                             (dialog "<c:Traveller:23>Goodbye, and good luck!")
                             (map (lambda (chr)
                                    (if (equal id (lookup 'id (cddr chr)))
                                        (chr-del (player) (car chr) (cadr chr))))
                                  (chrs (player)))
                             (defn on-dialog-closed ()
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
