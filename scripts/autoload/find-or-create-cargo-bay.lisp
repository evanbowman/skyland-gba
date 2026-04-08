;;;
;;; autoload/find-or-create-cargo-bay.lisp
;;;

(tr-bind-current)

(lambda (callback)
  (let ((bays (cargo-bays (player))))
    (if bays
        (let ((bay (sample bays)))
          (callback (car bay) (cdr bay)))
        (let ((cb callback))
          (alloc-space 'cargo-bay)
          (sel-input 'cargo-bay
                     (tr "Place cargo-bay:")
                     (lambda (isle x y)
                       (sound "build0")
                       (room-new (player) (list 'cargo-bay x y))
                       (cb x y)))))))
