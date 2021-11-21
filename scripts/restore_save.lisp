;;;
;;; The game invokes this script when loading a save file. The script should
;;; contain a single s-expression, a lambda, which will receive data written by
;;; save.lisp.
;;;


(lambda
  (eval-other-file "reset_hooks.lisp")

  (let ((data $0))
    (let ((load (lambda (cdr (assoc $0 data)))))

      (if (equal (load 'save-protocol) 1)
          (progn
            (terrain (player) (load 'terrain))

            (configure-player (player) (load 'rooms))

            (map
             (lambda
               (add-chr (player)
                        (get $0 0) ;; x
                        (get $0 1) ;; y
                        'neutral
                        (if (> (length $0) 3)
                            (get $0 3) ;; 1/0 possibly in this index if chr is replicant
                          0))

               (if (> (length $0) 2)
                   (chr-hp (player) (get $0 0) (get $0 1) (get $0 2))))
             (load 'chrs))

            (def enemies-seen (load 'enemies-seen))
            (def frendlies-seen (load 'friendlies-seen))

            (def last-zone (load 'last-zone)))))))
