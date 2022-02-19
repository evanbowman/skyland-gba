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


(setq
 next
 (if (and (equal (length enemies-seen) 0) (equal (zone) 0))
     (progn
       (setq enemies-seen (cons 0 enemies-seen))
       (lambda (eval-file "/scripts/event/hostile/0/0.lisp")))
   ;; Sometimes, procedurally generate an enemy
   (if (< (choice 100) 40)
       (lambda
         (opponent-generate
          (cond
           ((and (equal (zone 0)) (< (length enemies-seen) 2)) 0)
           ((equal (zone) 0) 3)
           ((equal (zone) 1) 6)
           (true 12))))
     (let ((avail-levels (filter
                          (lambda
                            (setq temp $0)
                            (not (filter (lambda (equal temp $0)) enemies-seen)))
                          (gen
                           (get '(8 8 6 3) (zone)) ;; number of levels to select from
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

         (lambda
           (eval-file (string "/scripts/event/hostile/" (zone) "/" lv-num ".lisp"))))))))


;; Just so we avoid evaluating another file while already evaluating a
;; relatively large expression. Unlikely to be problematic, but as we're running
;; on a small embedded cpu, no need to keep objects alive longer than needed.
(next)

(unbind 'next)

(gc)
