
(lambda
  (eval-file "/scripts/reset_hooks.lisp")

  (flag-show (player) 0)

  (let ((data $0))
    (let ((load (lambda (cdr (assoc $0 data)))))
      (if (> (load 'save-protocol) 2)
          (progn
            (terrain-set (player) (car (load 'terrain)))
            (opponent-init (cdr (load 'terrain)) 'hostile)

            (island-configure (player) (car (load 'rooms)))
            (island-configure (opponent) (cdr (load 'rooms)))

            (let ((setc
                   (lambda
                     (let ((isle $0)
                           (clst $1)
                           (type $2))
                       (map
                        (lambda
                          (let ((plst (cddr $0)))
                            (let ((chr -1))
                              (setq chr (chr-new isle
                                                 (get $0 0) ;; x
                                                 (get $0 1) ;; y
                                                 type
                                                 plst))

                              (let ((hp (assoc 'hp plst)))
                                (if hp
                                    (chr-hp chr (cdr hp))))

                              (chr-id chr (cdr (assoc 'id plst))))))

                        clst)))))

              (setc (player) (car (load 'chrs)) 'neutral)
              (setc (opponent) (cdr (load 'chrs)) 'hostile)))))))
