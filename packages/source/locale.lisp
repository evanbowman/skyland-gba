;;;
;;; locale.lisp
;;;

(defn/c tr-bind ((path . string))
  (let ((full-path (string "/strings/locale/" (lang) path)))
    (when (file-exists? full-path)
      (let ((bindings (eval-file full-path)))
        (when bindings
          (setq tr-bindings (append bindings tr-bindings)))))))

(defn/c tr-reset ()
  (setq tr-bindings nil))

(defn/c tr-load (text)
  (if (list? text)
      (map tr-load text)
      (let ((translation (lookup text tr-bindings)))
        (if (string? translation)
            translation
            text))))
