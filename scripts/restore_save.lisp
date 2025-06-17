;;;
;;; The game invokes this script when loading a save file. The script must
;;; return a function, which receives and processes the save data.
;;;


(lambda (data)
  (eval-file "/scripts/reset_hooks.lisp")

  (flag-show (player) 0)

  (eval-file "/scripts/adventure_vars.lisp")

  (let ((load (let ((d data))
                (lambda (key) (lookup key d)))))


    (setq adventure-log (if (> (load 'save-protocol) 3)
                            (load 'adventure-log)
                            '()))

    (wg-nav-path-set (load 'nav))

    (when (> (load 'save-protocol) 2)
      (terrain-set (player) (load 'terrain))

      (island-configure (player) (load 'rooms))

      (setq qvar (if (assoc 'qvar data)
                     (load 'qvar)
                     '()))

      (map (lambda (cnfo)
             (let ((plst (cddr cnfo)))
               (let ((chr -1))
                 (setq chr (chr-new (player)
                                    (get cnfo 0) ;; x
                                    (get cnfo 1) ;; y
                                    'neutral
                                    plst))

                 (let ((hp (assoc 'hp plst)))
                   (if hp
                       (chr-hp chr (cdr hp))))

                 (chr-id chr (lookup 'id plst)))))

           (load 'chrs))

      (setq enemies-seen (load 'enemies-seen))
      (setq friendlies-seen (load 'friendlies-seen))
      (setq surprises-seen (load 'surprises-seen))

      (setq quests (load 'quests))

      (setq last-zone (load 'last-zone))

      (setq qids (load 'qids))
      (setq pending-events (load 'pending-events))
      (setq zone-shop-items (load 'zone-shop-items)))

    (let ((grp (load 'groups))
          (join (lambda (g)
                  (let ((grp g))
                    (lambda (xy) (groups-add grp (first xy) (second xy)))))))
      (when grp
        (groups-reset)
        (map (join 'Up) (lookup 'Up grp))
        (map (join 'Left) (lookup 'Left grp))
        (map (join 'Right) (lookup 'Right grp))
        ;; See also: powerdown_allowed config in init.lisp
        (map (lambda (xy)
               (poweroff (player) (first xy) (second xy) true))
             (lookup 'poweroff grp))))

    (let ((fact (load 'faction)))
      (if fact
          (faction-set fact)))

    (when (> (load 'save-protocol) 1)
      (difficulty-set (load 'diff)))))
