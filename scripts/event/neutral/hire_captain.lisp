
(dialog "You reach a small port city! Buzzing with activity, dozens of other fortresses are docked nearby...")


(opponent-init 12 'neutral)

(island-configure
 (opponent)
 '((lemon-tree 0 11)
   (masonry 0 14 0)
   (masonry 0 13 0)
   (masonry 1 14 0)
   (masonry 1 13 2)
   (power-core 2 11)
   (masonry 2 13 0)
   (masonry 2 14 2)
   (bronze-hull 3 14)
   (water-source 4 14)
   (bridge 4 12)
   (bronze-hull 5 14)
   (masonry 6 10 0)
   (manufactory 6 11)
   (masonry 6 13 3)
   (masonry 6 14 3)
   (windmill 6 9)
   (masonry 7 14 3)
   (masonry 7 13 3)
   (masonry 7 10 0)
   (masonry 7 9 0)
   (masonry 8 14 3)
   (masonry 8 13 3)
   (crane 9 13)
   (lemon-tree 9 10)
   (bronze-hull 9 12)
   (shrubbery 10 10)
   (bronze-hull 10 11)
   (bronze-hull 10 12)))


(defn on-converge
  (setq on-converge nil)
  (dialog "While exploring, several experienced captains offer to lend their expertise. Your crew needs help deciding which to hire...")
  (defn on-dialog-closed
    (setq on-dialog-closed nil)
    (capn-select)))


(defn on-capn-sel
  (let ((icon $0)
        (name $1))
    (adventure-log-add 52 (list name))
    (let ((slot (chr-slots (player))))
      (if slot
          (progn
            (setq slot (get slot (choice (length slot))))
            (chr-new (player)
                     (car slot)
                     (cdr slot)
                     'neutral
                     (list (cons 'icon icon)))
            (dialog "A new captain joined your crew!")
            (exit))
        (progn
          (while (< (length (construction-sites (player) '(1 . 2))) 1)
            (terrain (player) (+ (terrain (player)) 1)))
          (sel-input 'ladder
                     "Place captain:"
                     (lambda
                       (sound "build0")
                       (room-new (player) `(ladder ,$1 ,$2))
                       (chr-new (player)
                                $1
                                (+ 1 $2)
                                'neutral
                                (list (cons 'icon icon)))
                       (dialog "A new captain joined your crew!")
                       (exit))))))))
