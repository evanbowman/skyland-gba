;;;
;;; neutral/2/6_1.lisp
;;;

(dialog
 "A proximity alarm wakes your crew in the night! <B:0>"
 "<b:/scripts/data/img/night.img.bin>"
 "An unknown island appears in the moonlight. <B:0>"
 "You almost ran right into it! Upon contacting them, you find the inhabitants in a heated discussion...")

(weather 7)

(opponent-init 10 'neutral)

(island-configure
 (opponent)
 '((shrubbery 0 14) (bronze-hull 1 13) (shrubbery 1 12) (masonry 1 14 0) (masonry 2 14 0) (power-core 2 11) (masonry 3 14 0) (power-core 4 8) (power-core 4 12) (masonry 4 14 0) (power-core 4 10) (masonry 5 14 0) (masonry 6 14 0) (reactor 6 10) (masonry 6 13 0) (windmill 7 14) (masonry 7 13 0) (masonry 8 14 0) (masonry 8 13 0) (bronze-hull 8 12) (lemon-tree 8 10) (shrubbery 9 14)))


(defn on-converge ()
  (dialog
   "<c:king:27>As this storm approaches, we keep getting horrible transmissions from islands that fall into the bad weather. <B:0> We're debating what to do with our kingdom's arsenal of atomic weapons... we wouldn't want them to fall into the wrong hands. <B:0> We've heard good news about your battles with the goblin horde! Can you take our atomics for safe keeping?")
  (setq on-converge nil)
  (alloc-space 'incinerator)
  (adventure-log-add 46 '())
  (sel-input 'warhead
             "Place weapon (1x2)"
             (lambda (isle x y)
               (room-new (player) (list 'warhead x y))
               (sound "build0")
               (dialog "<c:king:27> Hopefully you will never need to use this weapon. If you do, remember that it can only be fired once!")
               (setq on-dialog-closed exit))))
