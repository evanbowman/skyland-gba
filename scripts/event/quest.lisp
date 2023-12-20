;;;
;;; quest.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")

(gc)

(let ((opts (range 8)))
  (let ((lv 0)
        (lvs
         ;; Collect all quest ids not in the qids (seen) list
         (filter (lambda
                   (not (filter (equalto? $0) qids)))
                 opts)))

    (when lvs
      (setq lv (sample lvs)))

    (eval-file
     (format "/scripts/event/quest/%.lisp" lv))))


(gc)
