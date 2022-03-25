;;;
;;; quest_marker.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(gc)

(let ((z (zone))
      (pos (cdr (wg-pos))))
  (let ((found (filter
                (lambda
                  (equal pos (cdr (cdr $0))))
                quests)))
    (if found
        (progn
          (eval-file (car (car found))))
      (syscall "fatal" "invalid quest marker!"))))

(gc)
