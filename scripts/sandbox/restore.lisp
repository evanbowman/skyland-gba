
(lambda
  (eval-file "/scripts/reset_hooks.lisp")

  (flag-show (player))

  (let ((data $0))
    (let ((load (lambda (cdr (assoc $0 data)))))
      (if (> (load 'save-protocol) 2)
          (progn
            (terrain (player) (car (load 'terrain)))
            (opponent-init (cdr (load 'terrain)) 'hostile)

            (island-configure (player) (car (load 'rooms)))
            (island-configure (opponent) (cdr (load 'rooms)))

            (setq chr-names (load 'chr-names))

            (let ((setc
                   (lambda
                     (let ((isle $0)
                           (clst $1)
                           (type $2))
                       (map
                        (lambda
                          (let ((plst (cdr (cdr $0))))
                            (let ((chr -1))
                              (setq chr (chr-new isle
                                                 (get $0 0) ;; x
                                                 (get $0 1) ;; y
                                                 type
                                                 ;; Check if replicate
                                                 (if (assoc 'rplc plst) 1 0)))

                              (let ((hp (assoc 'hp plst)))
                                (if hp
                                    (chr-hp chr (cdr hp))))

                              (chr-id chr (cdr (assoc 'id plst))))))

                        clst)))))

              (setc (player) (car (load 'chrs)) 'neutral)
              (setc (opponent) (cdr (load 'chrs)) 'hostile)))))))