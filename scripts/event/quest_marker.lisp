;;;
;;; quest_marker.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(gc)

(let ((z (zone))
      (pos (cdr (wg-pos))))
  (let ((found (filter
                (lambda (q)
                  (equal pos (cddr q)))
                quests)))
    (if found
        (eval-file (string "/scripts/event/quest_marker/" (caar found)))
      (fatal "invalid quest marker!"))))

(gc)
