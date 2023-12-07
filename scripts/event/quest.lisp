;;;
;;; quest.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")

(gc)

(setq lv 0)

(let ((lvs
       ;; Collect all quest ids not in the qids (seen) list
       (filter (lambda
                 (not (filter (equalto? $0) qids)))
               (range 6))))
  (when lvs
    (setq lv (sample lvs))))

(eval-file
 (format "/scripts/event/quest/%.lisp" lv))

(unbind 'lv)

(gc)
