;;
;; Some variables used to keep state in adventure mode.
;;

(setq adventure-vars
      '(("mercenary-event" . nil)))

(defn/c adv-var-load ((name . string))
  (lookup name adventure-vars))

(defn/c adv-var-store ((name . string) val)
  (let ((n name))
    (setq adventure-vars (replace adventure-vars
                                  (lambda (kvp)
                                    (equal (car kvp) n))
                                  (cons name val)))))


(if (equal (difficulty) 0)
    (setvar "powerdown_allowed" 1)
    (setvar "powerdown_allowed" 0))
