;;;
;;; quest/sylph/3.lisp
;;;

(dialog
 "While passing a Sylph radio relay, you begin receiving a coded message... <B:0> The signal stutters and repeats, its pattern suggesting automated distress protocols rather than live transmission...")

(opponent-init 5 'neutral)

(island-configure
 (opponent)
 '((masonry 0 12 0) (bronze-hull 0 13) (shrubbery 0 11) (bronze-hull 0 14) (bronze-hull 1 12) (bronze-hull 1 11) (bronze-hull 1 10) (bronze-hull 1 9) (power-core 1 13) (bronze-hull 2 9) (canvas 2 12 (33 1997504648 1088867515 2134903394 -534774653 2130446335 -1909652418 26289095 540947848 14)) (deflector 2 10) (canvas 2 11 (33 -482639617 1846804497 -2138302153 -463465788 2046291592 1091356828 2146438984 -534774653 255)) (ladder 3 13) (bronze-hull 3 12) (bronze-hull 3 11) (bronze-hull 3 10) (bronze-hull 3 9) (canvas 3 8 (41 1999057920 -1008335093 1544007799 33496894 -503233784 1140449088 386858369 -4243500 12848896 805836881 64)) (bronze-hull 4 13) (bronze-hull 4 14) (masonry 4 12 0)))

(defn on-converge ()
  (dialog
   "<c:Sylph Relay:25>This is Research Platform Delta broadcasting emergency evacuation request. <B:0> Current situation: core crystal degradation exceeded safety thresholds forty-two minutes ago. Platform now operating on reserve power. <B:0> Personnel status: three researchers remain on station. Primary evacuation vessel delayed due to storm interference. <B:0> Calculating... nearest Conclave rescue ship is eight hours distant. Our power reserves project complete failure in approximately six hours. <B:0> Your vessel appears within range. We are transmitting precise coordinates and approach vectors. <B:0> Time is... a factor.")

  (defn on-dialog-closed ()
    (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
      (setq on-dialog-closed exit)
      (if m
          (progn
            (push 'qids 3)
            (push 'quests (cons "sylph_refugees.lisp" m))
            (adventure-log-add 19 '())
            (dialog "The coordinates lock into your navigation system. The platform's location appears on your sky chart, marked with an *. <B:0> The signal continues its automated loop, counting down the remaining power reserves..."))
          (progn
            (dialog "<c:Sylph Relay:25>Update: Research Platform Delta has ceased all transmissions. <B:0> Last recorded message indicated structural failure. <B:0> Evacuation objective... cancelled. <B:0> Recording incident for archive review."))))))
