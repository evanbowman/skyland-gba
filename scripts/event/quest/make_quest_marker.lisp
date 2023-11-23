

(let (;; c = corrupt nodes. A Node becomes corrupt when it falls into the storm.
      (c (filter
          (lambda (equal (car $0) 4))
          (wg-nodes)))
      (pos (wg-pos)))

  ;; Sort corrupt nodes by x-value. We don't want to place a quest marker too
  ;; close to the storm frontier.
  (if c
      (setq c (sort c (lambda
                        (> (cadr $0) (cadr $1))))))

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
               (> (cadr $0)
                  (if c
                      (+ 6 (cadr (car c)))
                    4))))
            (wg-nodes))))

    (when (> (length n) 4)
      (setq n (map (lambda
                     (cons $0
                           ;; Append manhattan length
                           (+ (abs (- (cadr $0) (cadr pos)))
                              (abs (- (cadr $0) (cadr pos))))))
                   n))
      (setq n (sort n (lambda (> (cdr $0) (cdr $1)))))
      (let ((n (car (get n (choice 3)))))
        (wg-node-set
         (car (cdr n))
         (cdr (cdr n))
         10)
        n))))
