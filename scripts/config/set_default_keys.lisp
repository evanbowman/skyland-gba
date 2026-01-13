;; Extract default keys from the settings template and splice them into the
;; current list of settings.

(let ((s1 (eval-file "/scripts/config/settings.lisp"))
      (s2 (settings-load)))
  (let ((key-defaults (filter (lambda (kvp)
                                (equal "key_" (slice (string (car kvp)) 0 4)))
                              s1)))
    (let ((other (difference key-defaults s1)))
      (settings-save (append key-defaults
                             (map (lambda (kvp)
                                    (assoc (car kvp) s2))
                                  other))))))
