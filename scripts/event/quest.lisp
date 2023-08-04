;;;
;;; quest.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")

(gc)

(setq lv 0)

(let ((lvs
       ;; Collect all quest ids not in the qids (seen) list
       (filter
        (lambda
          (let ((cmp $0))
            (not (filter (lambda (equal cmp $0)) qids))))
        (range 0 6))))
  (if lvs
      (setq lv (sample lvs))))

(eval-file
 (format "/scripts/event/quest/%.lisp" lv))

(unbind 'lv)

(gc)
