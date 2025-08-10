;;;
;;; neutral/1/10_goblin.lisp
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
  (dialog "<c:Villager:5>We're being raided! That goblin ship over there is slicing chunks off of our town! <B:0> They just let the scrap fall to the ruined world below, where their friends collect the fallen resources. <B:0> I know it's a long shot, but any chance you could help us?")

  (dialog-opts-reset)

  (let ((opty (lambda ()
                (opponent-mode 'hostile)
                (score-add 10000)
                (dialog "<c:Goblin:2> You think you can fight usss?! Hah!!")
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
                (dialog "<c:Goblin:2> HAAHAH! You made the right choice, not helping those nasssty humansss!")
                (defn on-dialog-closed ()
                  (setq on-dialog-closed exit)
                  (coins-add 1000)
                  (dialog "The goblins finish off the town, and share 1000@ of scrap with you.")))))

    (dialog-opts-push "Assist humans." opty)

    (dialog-opts-push "Do nothing." optn)

    (setq on-converge nil)))
