;;;
;;; neutral/1/10_human.lisp
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

(flag-show (opponent) flag-id-pirate)


(defn/temp defend-villagers ()
  (opponent-mode 'hostile)
  (await (dialog* "<c:Goblin:2>You think you can fight usss?! Hah!!"))
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
  (await (dialog* "One of the villagers came aboard to help out!")))


(defn/temp decline ()
  (await (dialog* "The goblins resume their attack. <B:0> "
                  "Maybe you weren't strong enough to face them anyway…"))
  (exit))


(defn/temp more-info ()
  (let ((sel (await (dialog-choice*
                     (string "<c:Villager:5>That goblin harvester is slicing chunks off of our "
                             "town! They just let the scrap fall to the ruined world below, "
                             "where their friends collect the fallen resources. "
                             "Anyway, can you help??")
                     '("Of course!"
                       "Sorry, but no.")))))
    (case sel
      (0 (defend-villagers))
      (1 (decline)))))


(defn on-converge ()
  (setq on-converge nil)
  (let ((sel (await (dialog-choice*
                     (string "<c:Villager:5>Oh! Just in time! Can you help us!? "
                             "That goblin ship over there is very strong, but there's "
                             "a chance you could defeat them...")
                     '("Of course!"
                       "What's going on here?"
                       "Sorry, I can't help.")))))
    (case sel
      (0 (defend-villagers))
      (1 (more-info))
      (2 (decline)))))
