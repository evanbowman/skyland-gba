

(let ((result 'nul)
      (rooms (eval-file "/scripts/config/room_tab.lisp"))) ;; load pricing info...
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
                    (setq result
                          (cons
                           (list $0 (+ (low $0) (choice 100)) $1)
                           result)))))

        (push 'hull 5 '(1 . 1))
        (push 'cannon 2 '(1 . 1))
        (push 'incinerator 1 '(2 . 2))

        result))))
