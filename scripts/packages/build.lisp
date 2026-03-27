;;;
;;; packages/build.lisp
;;;
;;; Compiles the lisp packages in this directory to bytecode.
;;;


(defn/temp compile-package (compile-fn eval-fn path library-path linkage)
  (log (string "Building: " path ", output: " library-path ", linkage: " linkage))
  (let ((store-fns nil))
    (let ((setfn (lambda (sym fn)
                   (global sym)
                   (set sym fn)
                   (setq store-fns (cons sym store-fns)))))
      (eval-fn path)
      (apply (curry compile-fn library-path linkage)
             store-fns)
      (setq setfn nil))))

;; NOTE: the game invokes build.lisp and then calls the lambda returned by the
;; script.
(lambda (compile-fn eval-fn input-paths output-path)
  (foreach
   (lambda (path)
     (when (ends-with path ".lisp")
       (let ((dirs (split path "/")))
         (let ((file (get dirs (decr (length dirs)))))
           (let ((fname (car (split file "."))))
             (compile-package compile-fn
                              eval-fn
                              path
                              (string output-path "/" fname ".slb")
                              'relocatable))))))
   input-paths))
