;;;
;;; locale.lisp
;;;

(defn/c tr-bind ((path . string))
  (let ((full-path (string "/strings/" (lang) path)))
    (when (file-exists? full-path)
      (when-let ((bindings (eval-file full-path)))
        (setq tr-bindings (append bindings tr-bindings))))))

(defn/c tr-bind-current ()
  (when-let ((path (eval '--current-file (caller-environment)))
             (path-sep "/"))
    (let ((new (string-join (cons "" (cdr (split path path-sep))) path-sep)))
      (tr-bind new))))

(defn/c tr-reset ()
  (setq tr-bindings nil)
  (tr-bind "/common.lisp"))

(defn/c tr-load (text)
  (cond
    ((list? text)
     (map tr-load text))
    ((pair? text)
     (cons (tr-load (car text))
           (tr-load (cdr text))))
    ((string? text)
     (let ((translation (lookup text tr-bindings)))
       (if (string? translation)
           translation
           text)))
    (true
     text)))
