;;;
;;; neutral.lisp
;;;


(eval-other-file "reset_hooks.lisp")


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
                                     friendlies-seen))))
                     (gen
                      (get '(4 4 2 1) (zone)) ;; number of levels to select from
                                              ;; based on current zone
                      (lambda $0)))))
  (if (equal (length avail-levels) 1)
      (def friendlies-seen '()))


  (let ((lv-num (get avail-levels (choice (length avail-levels)))))
    (def friendlies-seen (cons lv-num friendlies-seen))

    (eval-other-file (string 'neutral '_ (zone) '_ lv-num '.lisp))))


(gc)
