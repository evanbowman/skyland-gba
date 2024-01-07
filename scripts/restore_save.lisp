;;;
;;; The game invokes this script when loading a save file. The script must
;;; return a function, which receives and processes the save data.
;;;


(lambda
  (eval-file "/scripts/reset_hooks.lisp")

  (flag-show (player) 0)

  (setq ash-storm-count 0) ; TODO: put this in save data...

  (let ((data $0))
    (let ((load (lambda (lookup $0 data))))


      (setq adventure-log (if (> (load 'save-protocol) 3)
                              (load 'adventure-log)
                            '()))

      (when (> (load 'save-protocol) 2)
        (terrain-set (player) (load 'terrain))

        (island-configure (player) (load 'rooms))

        (setq qvar (if (assoc 'qvar data)
                       (load 'qvar)
                     '()))

        (map
         (lambda
           (let ((plst (cddr $0)))
             (let ((chr -1))
               (setq chr (chr-new (player)
                                  (get $0 0) ;; x
                                  (get $0 1) ;; y
                                  'neutral
                                  plst))

               (let ((hp (lookup 'hp plst)))
                 (if hp
                     (chr-hp chr hp)))

               (chr-id chr (lookup 'id plst)))))

         (load 'chrs))

        (setq enemies-seen (load 'enemies-seen))
        (setq friendlies-seen (load 'friendlies-seen))

        (setq quests (load 'quests))

        (setq last-zone (load 'last-zone))

        (setq qids (load 'qids))
        (setq zone-shop-items (load 'zone-shop-items)))

      (let ((grp (load 'groups))
            (join (lambda
                    (let ((grp $0))
                      (lambda (groups-add grp (car $0) (cdr $0)))))))
        (when grp
          (groups-reset)
          (map (join 'Up) (lookup 'Up grp))
          (map (join 'Left) (lookup 'Left grp))
          (map (join 'Right) (lookup 'Right grp))))

      (when (> (load 'save-protocol) 1)
        (difficulty-set (load 'diff))))))
