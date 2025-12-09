;;;
;;; neutral/1/0_goblin.lisp
;;;


(dialog
 "High above the storm clouds, you notice a solitary watchtower drifting silently through the upper atmosphere... <B:0> A figure in bronze armor stands motionless at its peak, observing the chaos below with detached interest. <B:0> As you approach, they signal your fortress with precise, measured light pulses...")


(opponent-init 4 'neutral)


(island-configure
 (opponent)
 '((bronze-hull 0 14) (bronze-hull 0 8) (bronze-hull 0 13) (sylph-cannon 0 9) (bronze-hull 1 8) (masonry 1 11 3) (masonry 1 12 3) (masonry 1 13 3) (masonry 1 14 3) (power-core 1 9) (masonry 2 14 3) (masonry 2 13 3) (masonry 2 12 3) (masonry 2 11 3) (bronze-hull 2 8) (bronze-hull 3 14) (bronze-hull 3 13) (bronze-hull 3 8)))

(flag-show (opponent) flag-id-sylph)

(chr-new (opponent) 0 7 'neutral '((race . 4)))



(defn on-converge ()
  (dialog
   "<c:Sylph Sentry:47>The patterns below grow more chaotic with each passing cycle. <B:0> My observations are complete. The archive has been updated. <B:0> My current assignment concludes with the approach of the storm front. <B:0> Your vessel appears... adequate for continued surveillance duties. <B:0>"
   "I am trained in combat protocols and defensive systems analysis. <B:0> "
   (case (faction)
     ('sylph "<B:0>")
     (else "My people value knowledge above all else. Your journey may provide... useful data. <B:0>"))
   " Do you require additional crew?")

  (dialog-await-binary-q
   (format "Recruit? %@" (* 400 (zone)))
   "No thanks.")

  (setq on-converge nil))


(defn on-dialog-accepted ()
  (if (> (* 400 (zone)) (coins))
      (progn
        (dialog "You cannot afford to pay. The "
                (case (faction)
                  ('sylph "sentry")
                  (else "Sylph"))
                " becomes impatient, and cuts the transmission.")
        (exit))
      (run-util-script
       "find-crew-slot"
       "<c:Sylph Sentry:47>You're out of space. This is inconvenient, but I suppose I can help you out..."
       'ladder
       "Place block (1x2):"
       (lambda (x y _)
         (chr-del (opponent) 0 7)
         (chr-new (player) x y 'neutral '((race . 4) (icon . 47)))
         (coins-add (* -400 (zone)))
         (adventure-log-add 76 '())
         (dialog "<c:Sylph Sentry:47> Acceptable. I am prepared to serve.")
         (defn on-dialog-closed ()
           (setq on-dialog-closed exit)
           ;; (sound "click_digital_1")
           ;; TODO: fix rendering so I can draw this image... <b:/scripts/data/img/sentry_closeup.img.bin>
           (dialog "The sentry joined your crew!"))))))


(defn on-dialog-declined ()
  (dialog "<c:Sylph Sentry:47>Understood. Safe travels.")
  (defn on-dialog-closed ()
    (let ((gender (sample '(("her" "she") ("his" "he")))))
      (dialog "The figure returns to "
              (get gender 0)
              " vigil, bronze helmet gleaming in the cold light as "
              (get gender 1)
              " resumes silent vigil over the storms below..."))
    (setq on-dialog-closed exit))
  (adventure-log-add 76 '()))
