;;;
;;; packages/build.lisp
;;;
;;; Compiles the lisp packages in this directory to bytecode.
;;;


(defn build-library (path library-path linkage)
  (log (string "Building: " path ", output: " library-path ", linkage: " linkage))
  (let ((store-fns nil))
    (let ((setfn (lambda (sym fn)
                   (global sym)
                   (set sym fn)
                   (setq store-fns (cons sym store-fns)))))
      (eval-file path)
      (apply (curry save-library library-path linkage)
             store-fns)
      (setq setfn nil))))


(defn ends-with (str sufx)
  (let ((m1 (string-explode str))
        (m2 (string-explode sufx)))
    (equal (slice m1 (- (length m1) (length m2))) m2)))


(filesystem-walk
 "/scripts/packages/source/"
 (lambda (path)
   (when (ends-with path ".lisp")
     (let ((parsed (split (get (split path ".") 0) "/")))
       (let ((fname (get parsed (decr (length parsed))))
             (prefix-path (apply string
                                 (flatten
                                  (map (curry list "/")
                                       (cdr (slice parsed 0 (- (length parsed) 2))))))))
         (build-library path
                        (string prefix-path "/" fname ".slb")
                        'relocatable))))))
