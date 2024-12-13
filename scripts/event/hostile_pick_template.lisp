
(let ((lvs (filter
            (lambda (enmy)
              (let ((cmp enmy))
                (not (filter (lambda (lv)
                               (equal cmp lv))
                             enemies-seen))))
            (range
             ;; number of levels to select from based on current zone
             (get '(8 8 7 3) (zone))))))

  (if lvs
      (let ((lv (sample lvs)))

        ;; Ok, so if we're at the point where we've exhausted all of the possible
        ;; level scenarios (which shouldn't really happen, anyway), we should clear
        ;; the list of seen enemies, so that next time we won't end up with nil.
        (when (equal (length lvs) 1)
          (setq enemies-seen '()))

        (when (and (equal (length enemies-seen) 0)
                   (equal (zone) 0))
          (setq lv 0))

        (push 'enemies-seen lv)

        (lambda ()
          (eval-file (format "/scripts/event/hostile/%/%.lisp" (zone) lv))))
    ;; We've run out of level templates! Use procedural generation instead.
    procgen))
