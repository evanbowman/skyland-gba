;;;
;;; neutral/2/6_0.lisp
;;;


(dialog
 "<b:/scripts/data/img/burning_isle.img.bin> "
 "An acrid plume of smoke rises from a fortress on the horizon...")


(opponent-init 7 'neutral)

(island-configure
 (opponent)
 '((incinerator 0 13)
   (hull 2 14)
   (power-core 3 13)
   (hull 3 12)
   (hull 5 14)
   (torch 2 10)
   (torch 3 9)
   (torch 5 11)))

(defn on-fadein ()
  (foreach (lambda (room)
             (if (equal (get room 0) 'torch)
                 (fire-new (opponent) (get room 1) (get room 2))))
           (rooms (opponent)))
  (setq on-fadein nil))

;; (flag-show (opponent) 1)


(defn on-converge ()
  (dialog
   "Looks like a terrible battle happened here... The crew seems to have abandoned the burning island, leaving behind a powerful weapon...")
  (setq on-converge nil)
  (alloc-space 'incinerator)
  (adventure-log-add 46 '())
  (sel-input 'incinerator
             "Place weapon (2x2)"
             (lambda (isle x y)
               (room-new (player) (list 'incinerator x y))
                 (room-del (opponent) 0 13)
               (sound "build0")
               (dialog "A delicate weapon built long ago on the surface... protect it carefully, because you can't build a replacement.")
               (run-util-script "pickup-cart"
                                7
                                "Oh! You notice something else..."
                                exit))))
