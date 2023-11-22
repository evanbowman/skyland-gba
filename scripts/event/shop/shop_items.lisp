

(let ((ret '())
      (rooms (eval-file "/scripts/config/room_tab.lisp")) ;; load pricing info...
      (opts (eval-file (format "/scripts/event/shop/%/%.lisp"
                               (zone)
                               (choice 4)))))
  ;; Ok, so we want to make shop items cheaper than if players built them
  ;; manually, but not so cheap that you could increase your coin count by
  ;; salvaging the purchased blocks.
  (let ((find (lambda
                (let ((name $0))
                  (filter (lambda (equal (get $0 0) name)) rooms)))))

    (let ((low (lambda ;; minimum price that we'll sell at
                 (let ((r (car (find $0))))
                   (let ((cost (get r 2)))
                     ;; lower limit is 65 percent, the salvage factor
                     (/ (* cost 65) 100))))))

      (let ((push (lambda
                    ;; [room-sym qty size]
                    (setq ret
                          (cons
                           (list $0 (+ (low $0) (choice 100)) $1 $2)
                           ret)))))

        (map (lambda
               (push (car $0) (cdr $0) (rinfo 'size (car $0))))
             opts)

        ret))))
