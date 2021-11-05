;;;
;;; hostile.lisp
;;;
;;; Entry point for loading hostile enemy scenarios
;;;


(eval-other-file "reset_hooks.lisp")


(if (not (equal (zone) last-zone))
    (progn
      (set 'friendlies-seen '())
      (set 'enemies-seen '())))


(set 'last-zone (zone))


(let ((avail-levels (filter
                     (lambda
                       (set 'temp $0)
                       (not (length (filter
                                     (lambda (equal temp $0))
                                     enemies-seen))))
                     (gen
                      (get '(8 6 6 1) (zone)) ;; number of levels to select from
                                              ;; based on current zone
                      (lambda $0))))
      (lv-num (get avail-levels (choice (length avail-levels)))))

  ;; Ok, so if we're at the point where we've exhausted all of the possible
  ;; level scenarios (which shouldn't really happen, anyway), we should clear
  ;; the list of seen enemies, so that next time we won't end up with nil.
  (if (equal (length avail-levels) 1)
      (set 'enemies-seen '()))

  (if (equal (length enemies-seen) 0)
      (if (equal (zone) 0)
          (set 'lv-num 0)))

  (set 'enemies-seen (cons lv-num enemies-seen))

  (eval-other-file (string 'hostile '_ (zone) '_ lv-num '.lisp)))
