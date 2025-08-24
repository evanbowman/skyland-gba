;;;
;;; neutral/2/6_2.lisp
;;;


(dialog
 "A heavily damaged fortress appears through the mist. <B:0> "
 "<b:/scripts/data/img/sylph_ship.img.bin>"
 "Your crew is taken aback - few raiders dare attack Sylph vessels, and fewer still succeed...")


(opponent-init 10 'neutral)

(island-configure
 (opponent)
 '((bronze-hull 1 13 40)
   (sylph-cannon 1 12)
   (sylph-cannon 1 14)
   (bronze-hull 2 12 200)
   (bronze-hull 2 13)
   (bronze-hull 2 14)
   (bronze-hull 2 11 40)
   (sylph-cannon 2 10)
   (plundered-room 3 11)
   (power-core 3 13)
   (bronze-hull 3 10)
   (masonry 5 14 0)
   (plundered-room 5 12)
   (masonry 6 14 0)
   (plundered-room 6 12)
   (torch 6 10)
   (bronze-hull 7 14)
   (bronze-hull 8 14)
   (plundered-room 8 12)
   (bronze-hull 8 11)
   (torch 8 10)
   (bronze-hull 9 14)))


(flag-show (opponent) flag-id-sylph)

(defn on-fadein ()
  (foreach (lambda (room)
             (if (equal (get room 0) 'torch)
                 (fire-new (opponent) (get room 1) (get room 2))))
           (rooms (opponent)))
  (setq on-fadein nil))


(defn/temp remove-one (room-sym)
  (let ((sym room-sym)
        (erased false))
    (foreach (lambda (r)
               (when (not erased)
                 (when (equal (car r) sym)
                   (room-del (opponent) (get r 1) (get r 2))
                   (setq erased true))))
             (rooms (opponent)))))


(defn/temp collect-block (i remaining)
  (let ((rem remaining)
        (index i))
    (alloc-space 'sylph-cannon)
    (sel-input 'sylph-cannon
               (format "Place cannon %/%" index (+ (decr index) remaining))
               (lambda (isle x y)
                 (room-new (player) (list 'sylph-cannon x y))
                 (sound "build0")
                 (remove-one 'sylph-cannon)
                 (if (equal rem 1)
                     (progn
                       (dialog "The Sylph are known for their elegant engineering. <B:0> These cannons don't just blast through blocks - they resonate with them somehow, seeming to deal more damage to stronger structures. <B:0> With the cannons safely aboard, you leave the mysterious wreck behind...")
                       (run-util-script "pickup-cart" 9
                                        "One of your crewmembers hands over another item found aboard the Sylph vessel..."
                                        exit))
                     (collect-block (incr index) (decr rem)))))))


(defn on-converge ()
  (dialog "You find the fortress abandoned, but several of its distinctive resonance cannons remain intact... <B:0> Your crew begins loading them aboard...")
  (setq on-converge nil)
  (collect-block 1 3))
