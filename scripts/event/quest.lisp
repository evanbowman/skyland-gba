;;;
;;; quest.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(let ((opts (range quest-count)))

  (when (equal (faction) 'sylph)
    ;; Disable a few sylph quests until we can finish writing new scripts for
    ;; the sylph faction...
    (setq opts (difference opts '(0 4))))

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
