;;;
;;; neutral/1/3_1.lisp
;;;


(dialog
 "<b:/scripts/data/img/battle_scene.img.bin> "
 "You come upon the scene of a heated battle. <B:0> A goblin resource harvester vessel is attacking an unarmed village, its beams slicing through the town. <B:0> Noticing you, the goblins briefly stop attacking…")


(opponent-generate 5)
(let ((r (rooms (opponent))))
  ;; Shift all block's y coord up by 1
  (setq r (map (lambda (rm)
                 (list (get rm 0)
                       (get rm 1)
                       (- (get rm 2) 1)))
               r))

  ;; remove anything cropped off the top of the map
  (setq r (filter (lambda (rm)
                    (> (get rm 2) 4))
                  r))

  (island-configure (opponent)
                    ;; Attach a beam gun and hull block
                    (append r
                            '((hull 0 14)
                              (beam-gun 1 14)))))


;; fill in the rest of the empty space with stuff
(map (lambda (x)
       (room-new (opponent) `(masonry ,x 14)))
     (range 4 (terrain (opponent))))

(opponent-mode 'neutral)

(flag-show (opponent) 0)


(defn on-converge ()
  (dialog "<c:villager:5> Oh! Just in time! Can you help us!? That goblin ship over there is very strong, but there's a chance you could defeat them...")

  (dialog-opts-reset)

  (let ((opty (lambda ()
                (opponent-mode 'hostile)
                (dialog "<c:goblin:2> You think you can fight usss?! Hah!!")
                (defn on-dialog-closed ()
                  (dialog "One of the villagers came aboard to help out!")
                  (when (not (chr-slots (player)))
                    (alloc-space 'ladder)
                    (let ((s (construction-sites (player) '(1 . 2))))
                      (if s
                          (room-new (player) (list 'ladder (caar s) (cdr (car s)))))))
                  (let ((slot (chr-slots (player))))
                    (if slot
                        (chr-new (player)
                                 (caar slot)
                                 (cdr (car slot))
                                 'neutral
                                 '((race . 0) (icon . 5)))))
                  (setq on-dialog-closed nil))))
        (optn (lambda ()
                (dialog "The goblins resume their attack. <B:0> Maybe you weren't strong enough to face them anyway…")
                (exit))))

    (dialog-opts-push "of course!" opty)

    (dialog-opts-push "what's going on here?"
                      (lambda ()
                        (dialog-opts-reset)
                        (dialog "<c:villager:5> That goblin harvester is slicing chunks off of our town! They just let the scrap fall to the ruined world below, where their friends collect the fallen resources. Anyway, can you help??")
                        (dialog-opts-push "of course!" opty)
                        (dialog-opts-push "sorry, but no." optn)))

    (dialog-opts-push "sorry, I can't help" optn)

    (setq on-converge nil)))
