
(lambda ()
  (let ((args $V))
    (while args
      (cond
        ((lambda? (car args))
         ((car args)))
        ((string? (car args))
         (await (dialog* (car args)))))
      (setq args (cdr args)))))
