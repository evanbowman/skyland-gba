
;; Return 1 if yes, 0 if no, redisplay prompt with option removed if selecting
;; one of the lore options.
(lambda ((message . string) (texty . string) (textn . string) lore)
  (let ((result nil)
        (msg message)
        (lore-opts lore))
    (while (nil? result)
      (let ((sel (await (dialog-choice* msg `(,texty ,@(map car lore-opts) ,textn)))))
        (case sel
          (0
           (setq result 1))
          ((incr (length lore-opts))
           (setq result 0))
          (else
           (let ((opt (get lore-opts (decr sel))))
             (setq msg (cdr opt))
             (setq lore-opts (remove lore-opts opt)))))))
    result))
