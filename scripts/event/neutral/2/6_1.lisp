;;;
;;; neutral/2/6_1.lisp
;;;


(dialog
 "An unexpected storm forces your island to retreat below the clouds... <B:0>"
 "Your scanners detect the presence of another island nearby... <B:0>"
 "<b:/scripts/data/img/deflector_city.img.bin>"
 "Shielded by a powerful deflector field, the fortress appears immune to damage from the harsh surface atmosphere. <B:0>"
 "They contact you offering assistance...")


(weather-set weather-id-ash)


(opponent-init 10 'neutral)

(island-configure
 (opponent)
 '((masonry 0 12 3) (masonry 0 14 3) (masonry 0 13 3) (masonry 1 14 3) (masonry 3 14 0) (deflector 3 11) (transporter 3 12) (masonry 4 14 3) (reactor 4 11) (masonry 5 14 3) (masonry 6 12 0) (power-core 6 13) (deflector 6 11) (masonry 8 14 3) (masonry 9 14 3) (masonry 9 13 3) (masonry 9 12 3)))

(flag-show (opponent) flag-id-marauder)


(defn on-converge ()
  (dialog "<c:Scavenger:29>Hello! Having trouble there? <B:0> Fortunately for you, we have a large stockpile of spare deflector fields! Here, take this one!")
  (setq on-converge nil)
  (alloc-space 'deflector)
  (sel-input 'deflector
             "Place deflector (1x1)"
             (lambda (isle x y)
               (room-new (player) (list 'deflector x y))
               (sound "build0")
               (dialog "<c:Scavenger:29>Careful with that shield, it's very old and handmade! <B:0> "
                       "We've been living near the radioactive ash for a long time, and we've managed to coexist with it! Unfortunately, some others were not so lucky...<B:0>"
                       "We have to be moving on, good luck!")
               (exit))))
