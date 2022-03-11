
(let ((avail-levels (filter
                     (lambda
                       (setq temp $0)
                       (not (filter (lambda (equal temp $0)) enemies-seen)))
                     (gen
                      (get '(8 8 7 3) (zone)) ;; number of levels to select from
                      ;; based on current zone
                      (lambda $0)))))

  (if avail-levels
      (let ((lv-num (get avail-levels (choice (length avail-levels)))))

        ;; Ok, so if we're at the point where we've exhausted all of the possible
        ;; level scenarios (which shouldn't really happen, anyway), we should clear
        ;; the list of seen enemies, so that next time we won't end up with nil.
        (if (equal (length avail-levels) 1)
            (setq enemies-seen '()))

        (if (equal (length enemies-seen) 0)
            (if (equal (zone) 0)
                (setq lv-num 0)))

        (push 'enemies-seen lv-num)

        (lambda
          (eval-file (string "/scripts/event/hostile/" (zone) "/" lv-num ".lisp"))))
    ;; We've run out of level templates! Use procedural generation instead.
    procgen))
