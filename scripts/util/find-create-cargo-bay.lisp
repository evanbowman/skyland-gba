;;;
;;; util/find-create-cargo-bay.lisp
;;;


(let ((c (cargo-bays (player))))
  (setq c (filter
           (lambda (xy)
             (not (cargo (player) (first xy) (second xy))))
           c))
  (cond
   ((equal (length c) 1)
    ;; Player has a single empty cargo bay.
    (car c))
   (c
    ;; Multiple empty cargo bays? Pick a random one.
    (get c (choice (length c))))
   (true
    ;; No cargo bays, try to create a new one.
    (setq c (construction-sites (player) '(1 . 2)))
    (if c
        (progn
          (setq c (car (sort c (lambda (xy1 xy2)
                                 (< (first xy1) (first xy2))))))
          (sound "build0")
          (room-new
           (player)
           (list 'cargo-bay (car c) (cdr c)))
          c)
      '()))))
