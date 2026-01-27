
(lambda ()
  (let ((t (this))
        (args $V))
    (cond
      ((lambda? (car args))
       ((car args))
       (apply t (cdr args)))
      ((string? (car args))
       (dialog (car args))
       (defn on-dialog-closed ()
         (setq on-dialog-closed nil)
         (if (cdr args)
             (apply t (cdr args))))))))
