;;;
;;; quest_marker.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(gc)

(let ((z (zone))
      (pos (cdr (wg-pos))))
  (let ((found (filter
                (lambda
                  (equal pos (cddr $0)))
                quests)))
    (if found
        (eval-file (caar found))
      (fatal "invalid quest marker!"))))

(gc)
