
(dialog
 "<b:/scripts/misc/img/ceramics.img.bin>A small village specializing in ceramics offers to sell you a batch of ornate tiles. Your crew insists that you'll be able to resell the tiles at another village for a much higher price...")


(opponent-init 13 'neutral)

(island-configure
 (opponent)
 '((bronze-hull 0 12)
   (bronze-hull 0 11)
   (bronze-hull 0 14)
   (bronze-hull 0 13)
   (bridge 1 12)
   (ladder 1 13)
   (masonry 2 9 0)
   (bronze-hull 2 11)
   (masonry 2 10 1)
   (power-core 2 13)
   (bridge 3 12)
   (masonry 3 11 1)
   (masonry 3 9 0)
   (masonry 3 10 1)
   (workshop 3 7)
   (water-source 4 14)
   (masonry 4 11 1)
   (masonry 4 9 0)
   (masonry 4 10 1)
   (coconut-palm 5 7)
   (bronze-hull 5 11)
   (masonry 5 14 1)
   (bridge 5 12)
   (masonry 5 9 0)
   (masonry 5 13 0)
   (masonry 5 10 1)
   (water-source 6 14)
   (windmill 6 13)
   (workshop 6 9)
   (stairwell 7 11)
   (shrubbery 8 13)
   (masonry 8 14 0)
   (masonry 9 14 0)
   (masonry 10 14 1)
   (hull 10 13)
   (statue 10 11)
   (masonry 11 14 1)
   (coconut-palm 11 12)
   (bronze-hull 12 14)
   (bronze-hull 12 13)
   (bronze-hull 12 12)
   (bronze-hull 12 11)
   (windmill 12 10)))


(flag-show (opponent) 6)


(let ((fee (cond
            ((< (scrap) 1000) (scrap))
            ((< (scrap) 8000) (/ (scrap) 2))
            (true 8000)))
      (qid 4))
  (defn on-converge [0]
    (setq on-converge nil)
    (dialog
     (format
      "<c:merchant:7>So, whaddya you say? Purchase a batch of ceramics for %@?"
      fee
      (* fee 2)))

    (dialog-await-binary-q "ok!" "no thanks")

    (defn on-dialog-accepted [0]
      (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp"))
            (c (eval-file "/scripts/util/find_create_cargo_bay.lisp")))
        (if (and m c)
            (progn
              (adventure-log-add 20 (list fee))
              (push 'qids qid)
              (push 'quests (cons "/scripts/event/quest_marker/ceramics.lisp" m))
              (push 'qvar (cons qid fee))
              (scrap-set (- (scrap) fee))
              (cargo-set (player) (car c) (cdr c) "ceramic tiles")
              (dialog "<c:merchant:7>Great, here are your tiles!")
              (defn on-dialog-closed [0]
                (dialog "(After talking with your crew, you mark the location of a town on your sky chart with an *)")
                (exit)
                (setq on-dialog-closed exit)))
          (progn
            (dialog
             "<c:merchant:7>Oh, I'm so sorry! We can't actually sell you anything today.")
            (setq on-dialog-closed exit)))))


    (setq on-dialog-declined
          (lambda
            (dialog "<c:merchant:7>No problem!")
            (setq on-dialog-closed exit)))))
