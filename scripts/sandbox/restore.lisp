;;;
;;; restore.lisp
;;;


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

            (when (lookup 'weather data)
              (weather-set (lookup 'weather data)))

            (let ((grp (load 'groups))
                  (join (lambda (g)
                          (let ((grp g))
                            (lambda (xy) (groups-add grp (first xy) (second xy)))))))
              (when grp
                (groups-reset)
                (map (join 'Up) (lookup 'Up grp))
                (map (join 'Left) (lookup 'Left grp))
                (map (join 'Right) (lookup 'Right grp))
                (map (lambda (xy)
                       (poweroff (player) (first xy) (second xy) true))
                     (lookup 'poweroff grp))))

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
