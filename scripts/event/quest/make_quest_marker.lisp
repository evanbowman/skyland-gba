

(let (;; c = corrupt nodes. A Node becomes corrupt when it falls into the storm.
      (c (filter (pos-equalto? 0 4)
                 (wg-nodes)))
      (pos (wg-pos)))

  ;; Sort corrupt nodes by x-value. We don't want to place a quest marker too
  ;; close to the storm frontier.
  (if c
      (setq c (sort c (lambda (n1 n2)
                        (> (cadr n1) (cadr n2))))))

  (let ((n (filter
            (lambda (node)
              (and
               (not (equal (car node) 0))
               (not (equal (car node) 4))
               (not (equal (car node) 5))
               (not (equal node pos))
               ;; The node must be somewhat ahead of the storm frontier; we
               ;; can't just place a node somewhere that's totally unreachable,
               ;; that's no fun.
               (> (cadr node)
                  (if c
                      (+ 6 (cadr (car c)))
                    4))))
            (wg-nodes))))

    (when (> (length n) 4)
      (setq n (map (lambda (node)
                     (cons node
                           ;; Append manhattan length
                           (+ (abs (- (cadr node) (cadr pos)))
                              (abs (- (cadr node) (cadr pos))))))
                   n))
      (setq n (sort n (lambda (kvp1 kvp2) (> (second kvp1) (second kvp2)))))
      (let ((n (car (get n (choice 3)))))
        (wg-node-set (cadr n)
                     (cddr n)
                     10)
        n))))
