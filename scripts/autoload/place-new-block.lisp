;;;
;;; autoload/place-new-block.lisp
;;;


(lambda (room-sym text)
  (alloc-space room-sym)
  (let (([x . y] (await (sel-input* room-sym text))))
    (sound "build0")
    (room-new (player) (list room-sym x y))
    (cons x y)))
