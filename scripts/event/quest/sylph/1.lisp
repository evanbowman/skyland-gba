;;;
;;; quest/sylph/1.lisp
;;;


(dialog
 "<b:/scripts/data/img/banana_cover.img.bin>"
 "You can't quite believe what you're seeing! A castle appears as if out of nowhere. Through the window, you see a man rushing about frantically, dressed in a banana suit!?")


(opponent-init 8 'neutral)

(island-configure
 (opponent)
 '((bronze-hull 0 13)
   (bronze-hull 0 14)
   (bronze-hull 0 12)
   (banana-plant 1 13)
   (masonry 1 14)
   (banana-plant 2 12)
   (masonry 2 13)
   (masonry 2 14)
   (masonry 3 14)
   (masonry 3 11)
   (power-core 3 9)
   (masonry 3 13)
   (masonry 3 12)
   (masonry 4 14)
   (masonry 4 13)
   (masonry 4 12)
   (masonry 4 11)
   (masonry 5 14)
   (masonry 5 13)
   (masonry 5 12)
   (banana-plant 5 11)
   (masonry 6 14)
   (masonry 7 14)))


(flag-show (opponent) flag-id-banana)


(secret
 4 13
 "I'm not going crazy, I'm not going crazy, I'm not going crazy...")

(secret
 5 14
 "BANANA! BANANAA!!! BANANA! BANANA?")


(setq on-converge
      (lambda ()
        (dialog
         "<c:Banana Man:8>Waaa! My precious b'nanas! Stolen by goblins! Why do I need them, you ask!? That's TOP SECRET! Help me teach those goblins a lesson?")
        (dialog-await-binary-q "Of course!" "I'm kind of busy…")

        (setq on-dialog-accepted
              (lambda ()
                (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
                  (setq on-dialog-closed exit)
                  (if m
                      (progn
                        (adventure-log-add 17 '())
                        (push 'qids 1)
                        (push 'quests (cons "nanas.lisp" m))
                        (dialog "<c:Banana Man:8>No time to waste! I know exactly where they've taken my bananas, and I marked the location on your sky chart with an *!"))
                    (progn
                      (dialog "Without warning, banana man became distracted by something and cut the transmission. Such a shame, he was interesting!"))))))

        (setq on-dialog-declined
              (lambda ()
                (dialog "<c:Banana Man:8>You won't help me!? _sigh_ Nobody helps Banana Man these days...")
                (setq on-dialog-closed exit)))))
