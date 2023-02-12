;;;
;;; neutral/1/8.lisp
;;;


(dialog "You discover the ruins of a forsaken town. I guess no one would mind if you scavenged some resources...")



(opponent-init 9 'neutral)


(island-configure
 (opponent)
 '((power-core 0 13)
   (manufactory 0 11)
   (masonry 2 14 0)
   (masonry 2 13 0)
   (water-source 3 14)
   (bridge 3 12)
   (masonry 4 14 0)
   (masonry 4 13 0)
   (bridge 5 12)
   (water-source 5 14)
   (power-core 6 9)
   (bronze-hull 6 11)
   (masonry 6 13 0)
   (masonry 6 14 0)
   (workshop 7 11)
   (masonry 7 13 0)
   (water-source 7 14)
   (masonry 8 13 0)
   (masonry 8 14 0)))


(defn on-converge
  (let ((c (choice 2)))
    (cond
     ((equal c 0)
      (let ((amt (+ 200 (choice 400))))
        (coins-add amt)
        (dialog
         (format
          "Looks like someone already got here first. You collect %@."
          amt))
        (exit)))
     ((equal c 1)
      (let ((opts '((workshop . (2 . 2))
                    (infirmary . (2 . 2))
                    (transporter . (1 . 2))
                    (manufactory . (3 . 2))
                    (backup-core . (2 . 2)))))
        (let ((pick (sample opts)))
          (dialog
           (format
            "After boarding, you find a completely intact %. Your crew asks you where to install it..." (car pick)))
          (defn on-dialog-closed
            (setq on-dialog-closed nil)
            (while (not (construction-sites (player) (cdr pick)))
              (terrain (player) (+ (terrain (player)) 1)))
            (sel-input
             (cdr pick)
             (format "Pick a slot (%x%)"
                     (car (cdr pick))
                     (cdr (cdr pick)))
             (lambda
               (room-new (player) `(,(car pick) ,$1 ,$2))
               (syscall "sound" "build0")
               (dialog "All done!")
               (setq on-dialog-closed exit))))))))))
