;;;
;;; neutral.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")
(eval-file "/scripts/event/check_zone.lisp")


(if (and (> (zone) 2) (not (cart-found? 0)) (not (choice 3)))
    (eval-file "scripts/event/neutral/3/dev_cameo.lisp")
  (let ((avail-levels (difference friendlies-seen
                                  (range
                                   ;; number of levels to select from based on
                                   ;; current zone
                                   (get '(6 11 7 2) (zone))))))

    (when (equal (length avail-levels) 1)
      (setq friendlies-seen '()))

    (let ((lv (get avail-levels (choice (length avail-levels)))))
      (setq friendlies-seen (cons lv friendlies-seen))

      (eval-file
       ;"scripts/event/neutral/0/1_0.lisp"
       (format "scripts/event/neutral/%/%.lisp" (zone) lv)
       ))))


(gc)
