;;;
;;; store_version.lisp
;;;

(let ((fname "/save/version.dat"))
  (file-unlink fname)
  (when-let ((vf (file-open fname)))
    (map (lambda (val i)
           (file-write! vf (* i 4) (int-to-bytes val)))
         (version)
         (range 4))
    (file-store vf)))
