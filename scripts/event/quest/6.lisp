
(dialog "<b:/scripts/data/img/ruins.img.bin>"
        "You discover the ruins of an ancient civilization. "
        "The island appears deserted, but just as you are about to leave, someone signals for help...")


(opponent-init 9 'neutral)

(island-configure
 (opponent)
 '((masonry 0 13 0) (masonry 0 12 0) (masonry 0 14 2) (masonry 1 14 0) (masonry 1 13 2) (sunflower 3 13) (masonry 3 14 0) (windmill 4 10) (masonry 4 12 3) (masonry 4 14 3) (masonry 4 13 3) (masonry 4 11 3) (masonry 5 14 0) (masonry 5 13 3) (masonry 5 12 3) (masonry 5 11 3) (masonry 5 10 3) (workshop 6 13) (masonry 6 12 2) (bronze-hull 8 14)))



(defn on-converge ()
  (dialog "A small injured boy begins speaking softly in an archaic language...")

  (defn on-dialog-closed ()
    (dialog "<c:injured boy:26> "
            "<S:1>i am the only survivor! can you help me get back home?")

    (defn on-dialog-closed ()
      (dialog "You can't understand a word he's saying. But he seems to want to join your crew.<B:0> Invite him aboard?")

      (setq on-dialog-closed nil)

      (dialog-await-binary-q-w/lore "yes" "I'll pass"
                                    '(("who might he be?" .
                                       "He looks like he might be a Sylph child. They've been up here a long time, but not much is known about them. <B:0> Invite him aboard?")))

      (defn on-dialog-accepted ()

        (let ((sl (chr-slots (player))))
          (when (not sl)
            (alloc-space 'ladder)
            (let ((site (construction-sites (player) '(1 . 2))))
              (sound "build0.raw")
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
