
(let ((c (cargo-bays (player))))
  (setq c (filter
           (lambda (not (cargo (get $0 1) (get $0 2))))
           c))
  (if c
      (get c (choice (length c)))
    (progn
      ;; Ok, so the player doesn't actually have any cargo bays... let's try to
      ;; create one...
      (setq c (construction-sites (player) '(1 . 2)))
      (if c
          (progn
            (setq c (car (sort c (lambda (< (car $0) (car $1))))))
            (sound "build0")
            (room-new
             (player)
             (list 'cargo-bay (car c) (cdr c)))
            c)
        '()))))
