
(dialog
 "You can't quite believe what you're seeing! A castle appears as if out of nowhere. Through the window, you see a man rushing about frantically, dressed in a banana suit!?")


(opponent-init 6 'neutral)

(island-configure
 (opponent)
 '((power-core 3 13)
   (coconut-palm 5 13)))



(setq on-converge
      (lambda
        (dialog
         "<c:banana man:8>Waaa! My precious 'nanas! Stolen by goblins! Why do I need them, you ask!? That's TOP SECRET! Help me teach those goblins a lesson?")
        (dialog-await-y/n)

        (setq on-dialog-accepted
              (lambda
                (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
                  (setq on-dialog-closed exit)
                  (if m
                      (progn
                        (push 'quests (cons "/scripts/event/quest_marker/nanas.lisp"
                                            m))
                        (dialog "<c:banana man:8>No time to waste! I know exactly where they've taken my bananas, and I marked the location on your sky chart with an *!"))
                    (progn
                      (dialog "Without warning, banana man became distracted by something and cut the transmission. Such a shame, he was interesting!"))))))

        (setq on-dialog-declined
              (lambda
                (dialog "<c:banana man:8>You won't help me!? _sigh_ Nobody helps banana man these days...")
                (setq on-dialog-closed exit)))))
