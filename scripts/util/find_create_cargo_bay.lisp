;;;
;;; util/find_create_cargo_bay.lisp
;;;
;;; TODO: Rename this file in accordance with the other files in the "util" directory. [recte util/find-create-cargo-bay.lisp]
;;;


(let ((c (cargo-bays (player))))
  (setq c (filter
           (lambda (xy)
             (not (cargo (player) (first xy) (second xy))))
           c))
  (cond
   ((equal (length c) 1)
    ;; player has a single empty cargo bay
    (car c))
   (c
    ;; multiple empty cargo bays? pick a random one
    (get c (choice (length c))))
   (true
    ;; no cargo bays, try to create a new one
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
