;;
;; Some variables used to keep state in adventure mode.
;;

(setq adventure-vars
      '(("ash-storm-count" . 0)
        ("mercenary-event" . nil)))

(defn/c adv-var-load (name)
  (lookup name adventure-vars))

(defn/c adv-var-store (name val)
  (let ((n name))
    (setq adventure-vars (replace adventure-vars
                                  (lambda (kvp)
                                    (equal (car kvp) n))
                                  (cons name val)))))
