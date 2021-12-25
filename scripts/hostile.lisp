;;;
;;; hostile.lisp
;;;
;;; Entry point for loading hostile enemy scenarios
;;;


(eval-other-file "/scripts/reset_hooks.lisp")


(if (not (equal (zone) last-zone))
    (progn
      (def friendlies-seen '())
      (def enemies-seen '())))


(def last-zone (zone))


(let ((avail-levels (filter
                     (lambda
                       (def temp $0)
                       (not (length (filter
                                     (lambda (equal temp $0))
                                     enemies-seen))))
                     (gen
                      (get '(8 6 6 1) (zone)) ;; number of levels to select from
                                              ;; based on current zone
                      (lambda $0)))))

  (let ((lv-num (get avail-levels (choice (length avail-levels)))))

    ;; Ok, so if we're at the point where we've exhausted all of the possible
    ;; level scenarios (which shouldn't really happen, anyway), we should clear
    ;; the list of seen enemies, so that next time we won't end up with nil.
    (if (equal (length avail-levels) 1)
        (def enemies-seen '()))

    (if (equal (length enemies-seen) 0)
        (if (equal (zone) 0)
            (def lv-num 0)))

    (def enemies-seen (cons lv-num enemies-seen))

    (eval-other-file (string "/scripts/event/" 'hostile '_ (zone) '_ lv-num '.lisp))))


(gc)
