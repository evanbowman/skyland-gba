;;;
;;; The game invokes this script when loading a save file. The script must
;;; return a function, which receives and processes the save data.
;;;


(lambda
  (eval-file "/scripts/reset_hooks.lisp")

  (flag-show (player) 0)

  (let ((data $0))
    (let ((load (lambda (cdr (assoc $0 data)))))


      (setq adventure-log (if (> (load 'save-protocol) 3)
                              (load 'adventure-log)
                            '()))

      (when (> (load 'save-protocol) 2)
        (terrain (player) (load 'terrain))

        (island-configure (player) (load 'rooms))

        (setq qvar (if (assoc 'qvar data)
                       (load 'qvar)
                     '()))

        (map
         (lambda
           (let ((plst (cdr (cdr $0))))
             (let ((chr -1))
               (setq chr (chr-new (player)
                                  (get $0 0) ;; x
                                  (get $0 1) ;; y
                                  'neutral
                                  plst))

               (let ((hp (assoc 'hp plst)))
                 (if hp
                     (chr-hp chr (cdr hp))))

               (chr-id chr (cdr (assoc 'id plst))))))

         (load 'chrs))

        (setq enemies-seen (load 'enemies-seen))
        (setq friendlies-seen (load 'friendlies-seen))

        (setq quests (load 'quests))

        (setq chr-names (load 'chr-names))

        (setq last-zone (load 'last-zone))

        (setq qids (load 'qids))
        (setq shop-items (load 'shop-items)))

      (when (> (load 'save-protocol) 1)
        (diff-set (load 'diff))))))
