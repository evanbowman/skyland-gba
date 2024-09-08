
(lambda (d)
  (eval-file "/scripts/reset_hooks.lisp")

  (flag-show (player) 0)

  (let ((data d))
    (let ((load (lambda (key) (lookup key data))))
      (if (> (load 'save-protocol) 2)
          (progn
            (terrain-set (player) (car (load 'terrain)))
            (opponent-init (cdr (load 'terrain)) 'hostile)

            (island-configure (player) (car (load 'rooms)))
            (island-configure (opponent) (cdr (load 'rooms)))

            (let ((setc
                   (lambda (i c t)
                     (let ((isle i)
                           (clst c)
                           (type t))
                       (map
                        (lambda (c)
                          (let ((plst (cddr c)))
                            (let ((chr -1))
                              (setq chr (chr-new isle
                                                 (get c 0) ;; x
                                                 (get c 1) ;; y
                                                 type
                                                 plst))

                              (let ((hp (lookup 'hp plst)))
                                (if hp
                                    (chr-hp chr hp)))

                              (chr-id chr (lookup 'id plst)))))

                        clst)))))

              (setc (player) (car (load 'chrs)) 'neutral)
              (setc (opponent) (cdr (load 'chrs)) 'hostile)))))))
