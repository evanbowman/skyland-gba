;;;
;;; hostile.lisp
;;;
;;; Entry point for loading hostile enemy scenarios
;;;


(eval-other-file "reset_hooks.lisp")


(if (not (equal (zone) last-zone))
    (set 'enemies-seen '()))


(set 'last-zone (zone))


(set 'avail-levels ;; list of unvisited levels
     (filter
      (lambda
        (set 'temp (arg 0))
        (not (length (filter
                      (lambda (equal temp (arg 0)))
                      enemies-seen))))
      (gen
       (get '(8) (zone)) ;; number of levels to select from based on current zone
       (lambda (arg 0)))))


;; Ok, so if we're at the point where we've exhausted all of the possible level
;; scenarios (which shouldn't really happen, anyway), we should clear the list
;; of seen enemies, so that next time we won't end up with nil.
(if (equal (length avail-levels) 1)
    (set 'enemies-seen '()))


(set 'lv-num
     (if (equal (length enemies-seen) 0)
         0
         (get avail-levels (cr-choice (length avail-levels)))))


(set 'enemies-seen (cons lv-num enemies-seen))


(eval-other-file (string 'hostile_ (zone) '_ lv-num '.lisp))


;; Just to save some memory...
(set 'avail-levels '())
(gc)
