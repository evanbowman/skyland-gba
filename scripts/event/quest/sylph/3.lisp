;;;
;;; quest/human/3.lisp
;;;


(dialog
 "While passing a Sylph radio relay, you begin receiving a coded message...")


(opponent-init 5 'neutral)

(island-configure
 (opponent)
 '((masonry 0 12 0) (bronze-hull 0 13) (shrubbery 0 11) (bronze-hull 0 14) (bronze-hull 1 12) (bronze-hull 1 11) (bronze-hull 1 10) (bronze-hull 1 9) (power-core 1 13) (bronze-hull 2 9) (canvas 2 12 (33 1997504648 1088867515 2134903394 -534774653 2130446335 -1909652418 26289095 540947848 14)) (deflector 2 10) (canvas 2 11 (33 -482639617 1846804497 -2138302153 -463465788 2046291592 1091356828 2146438984 -534774653 255)) (ladder 3 13) (bronze-hull 3 12) (bronze-hull 3 11) (bronze-hull 3 10) (bronze-hull 3 9) (canvas 3 8 (41 1999057920 -1008335093 1544007799 33496894 -503233784 1140449088 386858369 -4243500 12848896 805836881 64)) (bronze-hull 4 13) (bronze-hull 4 14) (masonry 4 12 0)))


(defn on-converge ()
  (dialog
   "<c:Sylph Relay:25>Emergency beacon from Research Platform Delta. <B:0> Status: evacuation required. Personnel: three. <B:0> Our nearest vessel: eight hours out. <B:0> Platform power projection: six hours remaining. <B:0> You're closer. Transmitting coordinates...")

  (defn on-dialog-closed ()
    (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
      (setq on-dialog-closed exit)
      (if m
          (progn
            (push 'qids 3)
            (push 'quests (cons "sylph_refugees.lisp" m))
            (adventure-log-add 19 '())
            (dialog "You record the destination your sky chart with an *!"))
          (progn
            (dialog "<c:Sylph Relay:25> Research Platform Delta has unexpectedly stopped transmitting. Objective cancelled."))))))
