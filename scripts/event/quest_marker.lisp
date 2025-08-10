;;;
;;; quest_marker.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(defn starts-with (str prefix)
  ;; Yeah, string handling is extremely minimal in this LISP
  ;; interpreter. Convert them to lists first.
  (let ((m1 (string-explode str))
        (m2 (string-explode prefix)))
    (equal (slice m1 0 (length m2)) m2)))


(let ((z (zone))
      (pos (cdr (wg-pos))))
  (let ((path "/scripts/event/quest_marker/")
        (found (filter
                (lambda (q)
                  (equal pos (cddr q)))
                quests)))
    (if (not found)
        (progn
          (if (is-developer-mode)
             (fatal (string "Invalid quest marker! " (wg-pos))))
          (if quests
              (setq found quests)
              (fatal "Quest marker with no active quests!?"))))

    (eval-file
     ;; NOTE: For backwards compatibility with old save files. We used to
     ;; store the whole path that we want to evaluate, now we just store the
     ;; script name.
     (if (starts-with (caar found) path)
         (caar found)
         (string path (caar found))))))


(unbind 'starts-with)
