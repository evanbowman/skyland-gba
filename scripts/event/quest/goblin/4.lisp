;;;
;;; quest/goblin/4.lisp
;;;


(dialog
 "<b:/scripts/data/img/ceramics.img.bin>"
 "You encounter a goblin salvage crew hauling up strange artifacts through a break in the clouds. Your crew recognizes them as valuable relics from the surface wars...")


(opponent-init 13 'neutral)

(island-configure
 (opponent)
 '((bronze-hull 0 11)
   (bronze-hull 0 12)
   (canvas 0 10 (42 -1154012160 813991645 -2032459746 -811713673 -65523612 -1945893696 -2067778806 12845555 -1057552656 -2013204733 2 64))
   (bronze-hull 0 13)
   (bronze-hull 0 14)
   (bridge 1 12)
   (ladder 1 13)
   (bronze-hull 2 11)
   (masonry 2 9 0)
   (power-core 2 13)
   (masonry 2 10 1)
   (masonry 3 11 1)
   (canvas 3 5 (31 523531264 2003226845 268222587 -2145304584 134278657 79739924 1544177667 48 124 0))
   (masonry 3 7 0)
   (masonry 3 10 1)
   (canvas 3 6 (22 -2143730816 -1274839028 1040385 12202879 -1123815185 192 14))
   (masonry 3 9 0)
   (bridge 3 12)
   (masonry 3 8 0)
   (masonry 4 10 1)
   (masonry 4 8 0)
   (masonry 4 9 0)
   (masonry 4 11 1)
   (masonry 4 7 0)
   (water-source 4 14)
   (canvas 4 5 (31 -1157921914 1962409487 134185253 -323374081 33571830 39882255 -1878029822 145 131 196))
   (canvas 4 6 (21 405930214 1063317007 -33546560 -553552793 -2139420704 28))
   (bridge 5 12)
   (masonry 5 10 1)
   (bronze-hull 5 11)
   (masonry 5 9 0)
   (coconut-palm 5 7)
   (masonry 5 13 0)
   (masonry 5 14 1)
   (workshop 6 9)
   (windmill 6 13)
   (water-source 6 14)
   (stairwell 7 11)
   (masonry 8 14 0)
   (shrubbery 8 13)
   (masonry 9 14 0)
   (masonry 10 14 1)
   (statue 10 11 0)
   (hull 10 13)
   (masonry 11 14 1)
   (coconut-palm 11 12)
   (bronze-hull 12 14)
   (bronze-hull 12 13)
   (bronze-hull 12 12)
   (bronze-hull 12 11)
   (windmill 12 10)))


(flag-show (opponent) flag-id-merchant)


(let ((fee (cond
            ((< (coins) 1000) (coins))
            ((< (coins) 8000) (/ (coins) 2))
            (true 8000)))
      (qid 4))
  (defn on-converge ()
    (setq on-converge nil)
    (dialog
     (format
      "<c:Scavenger:34>Found thessse rare artifactsss in the ruinsss below! Worth a fortune to the right collector. Only %@ for the lot!"
      fee
      (* fee 2)))

    (dialog-await-binary-q "OK!" "No thanks.")

    (defn on-dialog-accepted ()
      (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
        (if m
            (progn
              (push 'quests (cons "artifacts.lisp" m))
              (run-util-script
               "find-or-create-cargo-bay"
               (lambda (x y)
                 (adventure-log-add 20 (list fee))
                 (push 'qids qid)
                 (push 'qvar (cons qid fee))
                 (coins-set (- (coins) fee))
                 (cargo-set (player) x y "artifacts")
                 (dialog "<c:Scavenger:34>Careful with the ancient tech!")
                 (defn on-dialog-closed ()
                   (dialog "(Your crew marks a collector's location on your sky chart with an *)")
                   (exit)
                   (setq on-dialog-closed exit)))))
          (progn
            (dialog
             "<c:Scavenger:34>Bah, sssomeone else already claimed the artifactsss...")
            (setq on-dialog-closed exit)))))

    (setq on-dialog-declined
          (lambda ()
            (dialog "<c:Scavenger:34>Argh, off with you then!")
            (setq on-dialog-closed exit)))))
