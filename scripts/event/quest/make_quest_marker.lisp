

(let (;; c = corrupt nodes. A Node becomes corrupt when it falls into the storm.
      (c (filter
          (lambda (equal (car $0) 4))
          (wg-nodes)))
      (pos (wg-pos)))

  ;; Sort corrupt nodes by x-value. We don't want to place a quest marker too
  ;; close to the storm frontier.
  (if c
      (setq c (sort c (lambda
                        (> (car (cdr $0)) (car (cdr $1)))))))

  (let ((n (filter
            (lambda
              (and
               (not (equal (car $0) 0))
               (not (equal (car $0) 4))
               (not (equal (car $0) 5))
               (not (equal $0 pos))
               ;; The node must be somewhat ahead of the storm frontier; we
               ;; can't just place a node somewhere that's totally unreachable,
               ;; that's no fun.
               (> (car (cdr $0))
                  (if c
                      (+ 4 (car (cdr (car c))))
                    4))))
            (wg-nodes))))

    (if (> (length n) 4)
        (progn
          (setq n (map (lambda
                         (cons $0
                               ;; Append manhattan length
                               (+ (abs (- (car (cdr $0)) (car (cdr pos))))
                                  (abs (- (cdr (cdr $0)) (cdr (cdr pos)))))))
                       n))
          (setq n (sort n (lambda (> (cdr $0) (cdr $1)))))
          (let ((n (car (get n (choice 3)))))
            (wg-node-set
             (car (cdr n))
             (cdr (cdr n))
             10)
            n))
      nil)))
