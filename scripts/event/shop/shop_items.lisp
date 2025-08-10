;;;
;;; shop_items.lisp
;;;


(let ((ret '())
      (rooms (eval-file "/scripts/config/room_tab.lisp")) ;; Load pricing info...
      (opts (eval-file (format "/scripts/event/shop/%/%.lisp"
                               (zone)
                               (choice 4)))))
  ;; OK, so we want to make shop items cheaper than if players built them
  ;; manually, but not so cheap that you could increase your coin count by
  ;; salvaging the purchased blocks.
  (let ((find-entry (lambda (n)
                (let ((name n))
                  (filter (lambda (room)
                            (equal (get room 0) name))
                          rooms)))))

    (let ((low (lambda (rsym) ;; Minimum price that we'll sell at.
                 (let ((r (car (find-entry rsym))))
                   (let ((cost (get r 2)))
                     ;; Lower limit is 65 percent, the salvage factor.
                     (/ (* cost 65) 100))))))

      (let ((push-result (lambda (sym qty size)
                           (setq ret
                                 (cons
                                  (list sym (+ (low sym) (choice 100)) qty size)
                                  ret)))))

        (map (lambda (kvp)
               (push-result (first kvp) (second kvp) (rinfo 'size (first kvp))))
             opts)

        ret))))
