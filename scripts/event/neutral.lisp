;;;
;;; neutral.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(if (not (equal (zone) last-zone))
    (progn
      (setq friendlies-seen '())
      (setq enemies-seen '())))


(setq last-zone (zone))


(let ((avail-levels (filter
                     (lambda
                       (let ((cmp $0))
                         (not (filter
                               (lambda (equal cmp $0))
                               friendlies-seen))))
                     (gen
                      (get '(5 7 5 2) (zone)) ;; number of levels to select from
                                              ;; based on current zone
                      (lambda $0)))))

  (if (equal (length avail-levels) 1)
      (setq friendlies-seen '()))

  (let ((lv (get avail-levels (choice (length avail-levels)))))
    (setq friendlies-seen (cons lv friendlies-seen))

    (eval-file (string "/scripts/event/neutral/" (zone) "/" lv ".lisp"))))


(gc)
