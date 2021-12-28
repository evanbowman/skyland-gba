;;;
;;; neutral.lisp
;;;


(eval-other-file "/scripts/reset_hooks.lisp")


(if (not (equal (zone) last-zone))
    (progn
      (setq friendlies-seen '())
      (setq enemies-seen '())))


(setq last-zone (zone))


(let ((avail-levels (filter
                     (lambda
                       (setq temp $0)
                       (not (length (filter
                                     (lambda (equal temp $0))
                                     friendlies-seen))))
                     (gen
                      (get '(4 4 2 1) (zone)) ;; number of levels to select from
                      ;; based on current zone
                      (lambda $0)))))
  (if (equal (length avail-levels) 1)
      (setq friendlies-seen '()))


  (let ((lv-num (get avail-levels (choice (length avail-levels)))))
    (setq friendlies-seen (cons lv-num friendlies-seen))

    (eval-other-file (string "/scripts/event/neutral/" (zone) "/" lv-num ".lisp"))))


(gc)
