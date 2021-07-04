;;;
;;; neutral.lisp
;;;


(eval-other-file "reset_hooks.lisp")


(if (not (equal (zone) last-zone))
    (progn
      (set 'friendlies-seen '())
      (set 'enemies-seen '())))


(set 'last-zone (zone))


(set 'avail-levels ;; list of unvisited levels
     (filter
      (lambda
        (set 'temp (arg 0))
        (not (length (filter
                      (lambda (equal temp (arg 0)))
                      friendlies-seen))))
      (gen
       (get '(4 2 1) (zone)) ;; number of levels to select from based on current zone
       (lambda (arg 0)))))


(if (equal (length avail-levels) 1)
    (set 'friendlies-seen '()))


(set 'lv-num (get avail-levels (cr-choice (length avail-levels))))


(set 'friendlies-seen (cons lv-num friendlies-seen))


(eval-other-file (string 'neutral '_ (zone) '_ lv-num '.lisp))


(set 'avail-levels '())
(gc)
