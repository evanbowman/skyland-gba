;;;
;;; quest/sylph/3.lisp
;;;

(tr-bind-current)

(dialog
 (tr "While passing a Sylph radio relay, you begin receiving a coded message... <B:0> The signal stutters and repeats, its pattern suggesting automated distress protocols rather than live transmission..."))

(opponent-init 5 'neutral)

(island-configure
 (opponent)
 '((bronze-hull 0 14) (masonry 0 12 0) (bronze-hull 0 13) (shrubbery 0 11) (power-core 1 13) (bronze-hull 1 9) (bronze-hull 1 10) (bronze-hull 1 11) (bronze-hull 1 12) (canvas 2 5 (25 455885824 64548577 -317759309 779497436 -58228528 67215391 40)) (canvas 2 7 (37 1981219032 -2059268096 -577013861 -1187531895 536642509 -1795125095 -261676809 -456342939 676861954 192)) (bronze-hull 2 9) (canvas 2 11 (33 -482639617 1846804497 -2138302153 -463465788 2046291592 1091356828 2146438984 -534774653 255)) (canvas 2 12 (33 1997504648 1088867515 2134903394 -534774653 2130446335 -1909652418 26289095 540947848 14)) (deflector 2 10) (canvas 2 8 (52 82583040 617389104 -670793714 203022402 184632014 526518488 -2071920881 569532517 -1062732561 -368343042 -2130714849 671146503 140 4 24 32)) (canvas 2 6 (18 15480320 -133693664 268353855 -58490896 16 160)) (ladder 3 13) (bronze-hull 3 12) (bronze-hull 3 11) (bronze-hull 3 10) (bronze-hull 3 9) (bronze-hull 4 13) (bronze-hull 4 14) (masonry 4 12 0)))

(defn on-converge ()
  (dialog
   (tr "<c:Sylph Relay:25>This is Research Platform Delta broadcasting emergency evacuation request. <B:0> Current situation: core crystal degradation exceeded safety thresholds forty-two minutes ago. Platform now operating on reserve power. <B:0> Personnel status: three researchers remain on station. Primary evacuation vessel delayed due to storm interference. <B:0> Calculating... nearest Conclave rescue ship is eight hours distant. Our power reserves project complete failure in approximately six hours. <B:0> Your vessel appears within range. We are transmitting precise coordinates and approach vectors. <B:0> Time is... a factor."))

  (defn on-dialog-closed ()
    (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
      (setq on-dialog-closed exit)
      (if m
          (progn
            (push qids 3)
            (push quests (cons "sylph_refugees.lisp" m))
            (adventure-log-add 19 '())
            (dialog (tr "The coordinates lock into your navigation system. The platform's location appears on your sky chart, marked with an *. <B:0> The signal continues its automated loop, counting down the remaining power reserves...")))
          (progn
            (dialog (tr "<c:Sylph Relay:25>Update: Research Platform Delta has ceased all transmissions. <B:0> Last recorded message indicated structural failure. <B:0> Evacuation objective... cancelled. <B:0> Recording incident for archive review.")))))))
