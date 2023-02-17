;;;
;;; neutral.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")
(eval-file "/scripts/event/check_zone.lisp")


(if (and (> (zone) 2) (not (cart-found? 0)))
    (eval-file "scripts/event/neutral/3/dev_cameo.lisp")
  (let ((avail-levels (filter
                       (lambda
                         (let ((cmp $0))
                           (not (filter
                                 (lambda (equal cmp $0))
                                 friendlies-seen))))
                       (gen
                        (get '(6 10 7 2) (zone)) ;; number of levels to select from
                        ;; based on current zone
                        (lambda $0)))))

    (if (equal (length avail-levels) 1)
        (setq friendlies-seen '()))

    (let ((lv (get avail-levels (choice (length avail-levels)))))
      (setq friendlies-seen (cons lv friendlies-seen))

      (eval-file (format "scripts/event/neutral/%/%.lisp" (zone) lv)))))


(gc)
