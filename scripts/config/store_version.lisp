;;;
;;; store_version.lisp
;;;


(defn store-version (path ver)
  (when-let ((vf (file-open path)))
    (map (lambda (val i)
           (file-write! vf (* i 4) (int-to-bytes val)))
         ver
         (range 4))
    (file-store vf)))


(let ((fname "/save/version.dat")
      ;; The oldest known install version, in case we ever need to know...
      (oldest "/save/version-oldest.dat"))

  (when (not (file-exists? oldest))
    (store-version oldest (version)))

  (file-unlink fname)
  (store-version fname (version)))


(unbind 'store-version)
