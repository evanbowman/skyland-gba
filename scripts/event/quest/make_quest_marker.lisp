;;;
;;; make_quest_marker.lisp
;;;


(let ((opts (filter (lambda (node)
                      (and
                       (not (equal (car node) 0)) ;; null node
                       (not (equal (car node) 4)) ;; corrupt node
                       (not (equal (car node) 5)) ;; exit node
                       (not (equal (cdr node) (cdr (wg-pos))))))
                    (wg-nodes)))
      (pos (wg-pos)))

  ;; Filter out nodes that will become unreachable.
  (setq opts (filter (lambda (n)
                       (let ((xy (cdr n)))
                         (let ((turns-until-corrupted (wg-turns-remaining xy)))
                           ;; NOTE: +1 because path includes both endpoints.
                           (> (+ turns-until-corrupted 1) (length (wg-path (cdr pos) xy))))))
                     opts))

  ;; Sort options by x coord.
  (setq opts (sort opts (lambda (o1 o2) (> (cadr o1) (cadr o2)))))

  (let ((n (if (> (length opts) 3)
               ;; If there are a whole bunch of options, pick a random distant
               ;; one, otherwise, pick a random option.
               (get opts (choice 4))
               (sample opts))))
    (when n
      (wg-node-set (cadr n) (cddr n) 10)
      n)))
