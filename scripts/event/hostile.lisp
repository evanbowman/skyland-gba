;;;
;;; hostile.lisp
;;;
;;; Entry point for loading hostile enemy scenarios
;;;


(eval-file "/scripts/reset_hooks.lisp")


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
                                     enemies-seen))))
                     (gen
                      (get '(8 7 6 1) (zone)) ;; number of levels to select from
                      ;; based on current zone
                      (lambda $0)))))

  (let ((lv-num (get avail-levels (choice (length avail-levels)))))

    ;; Ok, so if we're at the point where we've exhausted all of the possible
    ;; level scenarios (which shouldn't really happen, anyway), we should clear
    ;; the list of seen enemies, so that next time we won't end up with nil.
    (if (equal (length avail-levels) 1)
        (setq enemies-seen '()))

    (if (equal (length enemies-seen) 0)
        (if (equal (zone) 0)
            (setq lv-num 0)))

    (setq enemies-seen (cons lv-num enemies-seen))

    (eval-file (string "/scripts/event/hostile/" (zone) "/" lv-num ".lisp"))))


(gc)
