;;;
;;; quest/goblin/6.lisp
;;;


(dialog "<b:/scripts/data/img/ruins.img.bin>"
        "You discover the ruins of an ancient civilization. "
        "The island appears deserted, but just as you are about to leave, someone signals for help...")


(opponent-init 9 'neutral)

(island-configure
 (opponent)
 '((canvas 0 10 (57 -600080146 -16584704 -213720833 233703040 -1059258272 2088771338 -792805897 -1599119575 -1015287564 268492487 2130919492 -515433288 25694191 1744895869 128)) (masonry 0 9 0) (masonry 0 12 0) (canvas 0 11 (28 -1149819136 -1198587666 134209567 -20889608 2004152335 15664576 143 188 22 96)) (masonry 0 13 0) (masonry 0 14 2) (banana-plant 1 8) (masonry 1 12 3) (masonry 1 13 2) (canvas 1 10 (38 -600080146 -541201664 2099249358 14552960 -167019920 -2130739217 1798782496 2145239173 -870697973 127 192)) (masonry 1 9 2) (masonry 1 14 0) (masonry 2 9 0) (lemon-tree 2 7) (masonry 2 12 3) (canvas 2 10 (46 -600080146 -16584704 14541055 264273688 1090254558 -266467898 -266421228 235405458 -1006502018 -939516290 -255780992 6 136)) (canvas 2 11 (28 -1149819136 -1198587666 134209567 -20889608 2004152335 15664576 143 188 22 96)) (banana-plant 3 11) (canvas 3 10 (20 -600080146 -83365888 -266462528 415301624 111 240 7 128)) (masonry 3 12 3) (sunflower 3 13) (masonry 3 14 0) (masonry 3 9 0) (power-core 4 10) (masonry 4 14 3) (masonry 4 12 3) (masonry 4 13 3) (masonry 5 14 0) (windmill 5 13) (masonry 5 12 3) (workshop 6 13) (masonry 6 12 2) (lemon-tree 6 10) (bronze-hull 8 14) (masonry 8 13 3) (lemon-tree 8 11)))



(defn on-converge ()
  (dialog "A small injured boy begins speaking softly in an archaic language...")

  (defn on-dialog-closed ()
    (dialog "<c:Injured Boy:26> "
            "<S:1>I am the only survivor! Can you help me get back home?")

    (defn on-dialog-closed ()
      (dialog "You can't understand a word he's saying. But he seems to want to join your crew.<B:0> Invite him aboard?")

      (setq on-dialog-closed nil)

      (dialog-await-binary-q-w/lore "Yes." "I'll pass."
                                    '(("Who might he be?" .
                                       "He looks like he might be a Sylph child. They've been up here a long time, but not much is known about them. <B:0> Invite him aboard?")))

      (defn on-dialog-accepted ()

        (let ((sl (chr-slots (player))))
          (when (not sl)
            (alloc-space 'ladder)
            (let ((site (construction-sites (player) '(1 . 2))))
              (sound "build0")
              (room-new (player) `(ladder ,(caar site) ,(cdar site)))))

          (setq sl (chr-slots (player)))
          (let ((id (chr-new (player)
                             (caar sl)
                             (cdar sl)
                             'neutral
                             '((icon . 26)))))
            (chr-hp id 128)
            (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
              (if m
                  (progn
                    (adventure-log-add 54 nil)
                    (push 'qids 6)
                    (push 'quests (cons "civ.lisp" m))
                    (push 'qvar (cons 6 id))
                    (dialog "The orphan boy joined your crew! <B:0> Upon discovering your sky chart, he marked a location with an *...")
                    (exit))
                (progn
                  (dialog "The injured boy joined your crew! Wonder where he came from...")
                  (exit)))))))

      (setq on-dialog-declined exit))))
