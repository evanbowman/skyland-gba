;;;
;;; make_quest_marker.lisp
;;;



(let ((opts (filter quest-marker-can-place (wg-nodes)))
      (pos (wg-pos)))

  ;; Filter out nodes that will become unreachable.
  (setq opts (filter (quest-marker-is-reachable pos)
                     opts))

  ;; Sort options by x coord.
  (setq opts (sort opts sky-chart-xsort-compare))

  (let ((n (if (> (length opts) 3)
               ;; If there are a whole bunch of options, pick a random distant
               ;; one, otherwise, pick a random option.
               (get opts (choice 4))
               (sample opts))))
    (when n
      (wg-node-set (cadr n) (cddr n) 10)
      n)))
