;;;
;;; cargo-bays.lisp
;;;

(lambda ((isle . wrapped))
  (let ((rooms-list (rooms isle)))
    (map (lambda (room)
           (cons (cadr room)
                 (cadr (cdr room))))
         (filter (car-equalto? 'cargo-bay) rooms-list))))
