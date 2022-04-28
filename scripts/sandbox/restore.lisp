
(lambda
  (eval-file "/scripts/reset_hooks.lisp")

  (flag-show (player))

  (let ((data $0))
    (let ((load (lambda (cdr (assoc $0 data)))))
      (if (> (load 'save-protocol) 0)
          (progn
            (terrain (player) (car (load 'terrain)))
            (opponent-init (cdr (load 'terrain)) 'hostile)

            (island-configure (player) (car (load 'rooms)))
            (island-configure (opponent) (cdr (load 'rooms)))

            (map
             (lambda
               (chr-new (player)
                        (get $0 0) ;; x
                        (get $0 1) ;; y
                        'neutral
                        (if (> (length $0) 3)
                            (get $0 3) ;; 1/0 possibly in this index if chr is replicant
                          0))

               (if (> (length $0) 2)
                   (chr-hp (player) (get $0 0) (get $0 1) (get $0 2))))
             (car (load 'chrs)))

            (map
             (lambda
               (chr-new (opponent)
                        (get $0 0) ;; x
                        (get $0 1) ;; y
                        'hostile
                        (if (> (length $0) 3)
                            (get $0 3) ;; 1/0 possibly in this index if chr is replicant
                          0))

               (if (> (length $0) 2)
                   (chr-hp (opponent) (get $0 0) (get $0 1) (get $0 2))))
             (cdr (load 'chrs))))))))
