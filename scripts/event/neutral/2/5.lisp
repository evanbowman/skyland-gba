;;;
;;; neutral/2/5.lisp
;;;


(dialog "A shop approaches! Looks like they specialize in selling various types of defenses...")



(push 'zone-shop-items
      (cons (wg-pos)
            '((mirror-hull    350 8 (1 . 1))
              (energized-hull 300 4 (1 . 1))
              (stacked-hull   400 4 (1 . 1))
              (deflector      2800 2 (1 . 1)))))

(let ((xy (cdr (wg-pos))))
  ;; switch the current map node to a shop node type
  (wg-node-set (first xy) (second xy) 7))


(eval-file "/scripts/event/shop/shop.lisp")



(opponent-init 8 'neutral)

(island-configure
 (opponent)
 '((shrubbery 0 13)
   (masonry 0 14)
   (shrubbery 1 14)
   (mirror-hull 2 11)
   (mirror-hull 2 12)
   (mirror-hull 2 13)
   (mirror-hull 2 14)
   (mirror-hull 3 12)
   (mirror-hull 3 14)
   (mirror-hull 3 13)
   (mirror-hull 3 11)
   (water-source 4 14)
   (power-core 5 13)
   (hull 6 12)
   (masonry 7 14)
   (masonry 7 13)
   (hull 7 12)))


(flag-show (opponent) flag-id-merchant)
(chr-new (opponent) 6 11 'neutral 0)
