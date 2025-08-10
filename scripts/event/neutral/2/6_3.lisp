;;;
;;; neutral/2/6_3.lisp
;;;


(dialog
 "Your navigational instruments begin behaving strangely, "
 "their readings pulled toward a point far below. <B:0> "
 "<b:/scripts/data/img/crashed_city.img.bin>"
 "Descending through the clounds, you catch glimpses of white stone and still-glowing runes. <B:0>"
 "You discover a fallen Sylph city, partially submerged in the ash...")

(weather-set 6)

(opponent-init 13 'neutral)
(mountain-terrain-mode (opponent) true)

(island-configure
 (opponent)
 '((bronze-hull 1 12 56) (masonry 1 14 0) (bronze-hull 1 13 56) (bronze-hull 2 14 56) (bronze-hull 3 14 56) (deflector 4 12) (masonry 4 13 0) (masonry 4 14 0) (war-engine 5 11) (masonry 7 10 0) (power-core 8 13) (masonry 8 12 0) (masonry 8 10 0) (masonry 8 11 0) (masonry 9 10 0) (masonry 9 9 0) (masonry 9 11 0) (masonry 9 12 0) (masonry 10 10 0) (masonry 10 9 0) (deflector 10 11) (phase-shifter 10 12) (masonry 11 14 0) (masonry 11 13 0) (masonry 11 12 0) (masonry 11 11 0)))


(defn on-converge ()
  (dialog "Though long deserted, ancient machinery still hums within the ruins, crystal matrices pulsing with unfamiliar energy. <B:0> Most of the city's systems have failed, but a few chambers still maintain their enigmatic purpose... <B:0> Among the ruins, you discover an intricate device of crystal and warm brass, its surfaces etched with flowing Sylph script.")
  (setq on-converge nil)
  (alloc-space 'phase-shifter)
  (sel-input 'phase-shifter
             "Place phase-shifter (1x3)"
             (lambda (isle x y)
               (room-new (player) (list 'phase-shifter x y))
               (sound "build0")
               (dialog "You picked up a rare piece of Sylph technology! What could it's function be?")
               (run-util-script "pickup-cart"
                                9
                                "One of your crewmembers hands over another item found aboard the Sylph city..."
                                exit))))
