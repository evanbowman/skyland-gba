;;;
;;; quest.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(let ((opts (range 9)))

  (let ((lv 0)
        (lvs
         ;; Collect all quest ids not in the qids (seen) list.
         (filter (lambda (qi)
                   (not (filter (equalto? qi) qids)))
                 opts)))

    (when lvs
      (setq lv (sample lvs)))

    (eval-file
     (format "/scripts/event/quest/%/%.lisp"
             (faction)
             ;8
             lv
             ))))
