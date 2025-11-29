;;;
;;; neutral/1/7.lisp
;;;


(dialog
 "<b:/scripts/data/img/ruins_isle.img.bin>"
 "You discover the ruins of a forsaken town. I guess no one would mind if you scavenged some resources...")



(opponent-init 9 'neutral)


(island-configure
 (opponent)
 '((power-core 0 13)
   (manufactory 0 11)
   (masonry 2 13 0)
   (masonry 2 14 0)
   (water-source 3 14)
   (bridge 3 12)
   (masonry 4 14 0)
   (masonry 4 13 0)
   (canvas 5 7 (40 -1166592256 862238733 -2013143879 -560137935 1182925152 970987781 537789632 -1338736633 50716733 71 220 0 224))
   (canvas 5 6 (21 -1073514496 133169182 -583328896 1077172230 1688211976 0))
   (bronze-hull 5 11)
   (masonry 5 9 2)
   (water-source 5 14)
   (bridge 5 12)
   (masonry 5 10 2)
   (masonry 5 8 0)
   (power-core 6 9)
   (bronze-hull 6 11)
   (masonry 6 13 0)
   (masonry 6 14 0)
   (workshop 7 11)
   (masonry 7 13 0)
   (water-source 7 14)
   (masonry 8 13 0)
   (masonry 8 14 0)))

(flag-show (opponent) flag-id-colonist)


(defn on-converge ()
  (setq on-converge nil)
  (let ((c (choice 6))
        (end (lambda ()
               (run-util-script "pickup-cart" 4
                                "Something else catches your attention. <d:500>.<d:500>.<d:500> a data cartridge!"
                                exit))))
    (cond
     ((equal c 0)
      (let ((amt (+ 200 (choice 400))))
        (coins-add amt)
        (dialog
         (format
          "Looks like someone already got here first. You collect %@."
          amt))
        (adventure-log-add 38 (list amt))
        (end)))
     (true
      (let ((opts '(power-core
                    infirmary
                    manufactory
                    incinerator
                    warhead
                    particle-lance
                    beam-gun
                    deflector
                    splitter)))
        (let ((pick (sample opts)))
          (dialog
           "After boarding, you find a completely intact "
           (rinfo 'name pick)
           ". Your crew asks you where to install it...")
          (adventure-log-add 38 (rinfo 'name pick))
          (defn on-dialog-closed ()
            (setq on-dialog-closed nil)
            (alloc-space pick)
            (sel-input
             pick
             (format "Pick a slot (%x%)"
                     (car (rinfo 'size pick))
                     (cdr (rinfo 'size pick)))
             (lambda (isle x y)
               (room-new (player) `(,pick ,x ,y))
               (sound "build0")
               (dialog "All done!")
               (end))))))))))
