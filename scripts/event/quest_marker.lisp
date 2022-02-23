;;;
;;; quest_marker.lisp
;;;


(let ((z (zone))
      (pos (cdr (wg-pos))))
  (let ((found (filter
                (lambda
                  (and
                   (equal z (car (car $0)))
                   (equal pos (cdr (car $0)))))
                quests)))
    (if found
        (progn
          (eval-file (cdr found)))
      (syscall "fatal" "invalid quest marker!"))))
