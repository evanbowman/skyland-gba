;;;
;;; locale.lisp
;;;

(defn/c tr-bind ((path . string))
  (let ((full-path (string "/strings/" (lang) path)))
    (when (file-exists? full-path)
      (let ((bindings (eval-file full-path)))
        (when bindings
          (setq tr-bindings (append bindings tr-bindings)))))))

(defn/c tr-bind-current ()
  (when-let ((path (eval '--current-file (caller-environment)))
             (path-sep "/"))
    (tr-bind (string-join (cons "" (cddr (split path path-sep))) path-sep))))

(defn/c tr-reset ()
  (setq tr-bindings nil))

(defn/c tr-load (text)
  (cond
    ((list? text)
     (map tr-load text))
    ((pair? text)
     (cons (tr-load (car text))
           (tr-load (cdr text))))
    (true
     (let ((translation (lookup text tr-bindings)))
       (if (string? translation)
           translation
           text)))))
