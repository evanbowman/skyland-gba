;;;
;;; util/place-new-block.lisp
;;;


(lambda (room-sym text callback)
  (alloc-space room-sym)
  (let ((cb callback)
        (rsym room-sym))
    (sel-input room-sym
               text
               (lambda (isle x y)
                 (sound "build0")
                 (room-new (player) (list rsym x y))
                 (cb x y)))))
