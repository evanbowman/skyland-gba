;;;
;;; neutral/2/3.lisp
;;;


(dialog "A small fortress hurtles through the air, with goblins in pursuit. The captain calls for help...")


(opponent-init 9 'neutral)


(island-configure
 (opponent)
 '((power-core 3 13)
   (power-core 3 11)
   (decimator 0 13)
   (plundered-room 5 13)
   (plundered-room 5 11)
   (plundered-room 6 13)
   (hull 8 14)
   (hull 8 13)
   (hull 8 12)))

(chr-new (opponent) 2 14 'neutral 0)


(defn on-converge
  (dialog "<c:captain:7> I managed to steal this decimator from some goblins, but they're catching up to me! I know... I could sell you the weapon! I'll install it on your island for @1500...")
  (setq on-converge nil)
  (dialog-await-y/n))


(setq on-dialog-declined exit)


(defn on-dialog-accepted
  (if (< (coins) 1500)
      (progn
        (dialog "<c:captain:7> Sorry, I went to all this trouble, I really can't sell you this tech for less than @1500.")
        (exit))
    (progn
      (coins-add -1500)

      ;; We wouldn't want the player to get into a position where there isn't
      ;; enough terrain to place the weapon! The game would get locked up. Just
      ;; give the player some terrain for free.
      (repeat 2
              (if (not (construction-sites (player) '(2 . 2)))
                  (terrain (player) (+ (terrain (player)) 1))))

      (sel-input
       '(2 . 2)
       "Place weapon where? (2x2)"
       (lambda
         (room-new (player) (list 'decimator $1 $2))
         (room-del (opponent) 0 13)
         (dialog "<c:captain:7> Ok, all finished! The weapon recharges quite slowly, but nothing's more destructive! You need to move one of your crew into the weapon, though, or it won't recharge."))))))
