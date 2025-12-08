;;;
;;; quest/sylph/2.lisp
;;;


(dialog "<b:/scripts/data/img/farm_colony.img.bin>A nearby farming colony requests assistance...")


(opponent-init 9 'neutral)

(island-configure
 (opponent)
 '((bronze-hull 0 12)
   (bronze-hull 0 14)
   (bronze-hull 0 13)
   (sunflower 0 11)
   (power-core 1 13)
   (bronze-hull 1 12)
   (bronze-hull 2 12)
   (bronze-hull 2 11)
   (lemon-tree 2 9)
   (canvas 3 12 (25 -16286720 1086390257 -50393323 117113150 515900256 -2146963456 8))
   (lemon-tree 3 13)
   (fountain 3 10)
   (bronze-hull 3 11)
   (canvas 4 12 (15 17071104 2013331393 2015235 240 7 128))
   (fountain 4 14)
   (lemon-tree 4 9)
   (bronze-hull 4 11)
   (lemon-tree 5 9)
   (canvas 5 12 (27 50756608 607125657 144246080 -926728824 -2122321905 -14679724 200 27 252))
   (lemon-tree 5 13)
   (bronze-hull 5 11)
   (lemon-tree 6 13)
   (canvas 6 12 (34 6585856 -16707408 150487038 -261520579 -522092552 1058540027 -1945112953 -565444224 0 128))
   (lemon-tree 6 9)
   (bronze-hull 6 11)
   (masonry 7 14 0)
   (masonry 7 13 2)
   (bronze-hull 7 12)
   (workshop 7 10)
   (masonry 8 14 0)
   (masonry 8 13 0)
   (bronze-hull 8 12)))


(defn on-converge ()
  (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp")))
    (if m
        (progn
          (dialog
           "<c:Farmer Meyer:9>Looks like a bad storm's coming this way! Hey, I have an idea! How about you do me a favor and relocate my orchard of lemon trees to my brother's island... I'll tell him to pay you 1400@ for each one that you successfully deliver!")

          (defn on-dialog-closed ()
            (map (lambda (room)
                  (if (equal (car room) 'lemon-tree)
                      (room-del (opponent) (get room 1) (get room 2))))
                 (rooms (opponent)))

            (push 'qids 2)
            (push 'quests (cons "lemons.lisp" m))

            (let ((reward 0))
              (map
               (lambda (xy)
                 ((room-new
                   (player)
                   (list 'lemon-tree (first xy) (second xy)))
                  (+= reward 1400)))
               (construction-sites (player) '(1 . 2)))

              (push 'qvar (cons 2 reward)))

            (adventure-log-add 18 (list (room-count (player) 'lemon-tree)))

            (dialog "<c:Farmer Meyer:9>Please take good care of them! I marked my brother's address on your sky chart with an *!")
            (defn on-dialog-closed ()
              (dialog "<c:Farmer Meyer:9>Anything else you'd like to discuss?")

              (let ((t (this)))
                (let ((chat (lambda (str)
                              (let ((s str))
                                (lambda ()
                                  (dialog s)
                                  (setq on-dialog-closed t))))))
                  (dialog-opts-push "Farming?"
                                    (chat "<c:Farmer Meyer:9>Farms up here grow crops cultivated for cold air. Even so, nothing would grow at this altitude without heat from the island reactor core..."))

                  (dialog-opts-push "Irrigation?"
                                    (chat "<c:Farmer Meyer:9>Although we do get some light rain up here, it's not enough for growing crops. But we can harvest water from the clouds, of course..."))

                  (dialog-opts-push "Nope!" (lambda ()
                                             (dialog "<c:Farmer Meyer:9>Good luck!")
                                             (setq on-dialog-closed exit)
                                             (exit)))))

              )))
      (progn
        (dialog "<c:Farmer Meyer:9>I was going to ask you to help relocate some of these lemon trees, but the storm's getting closer and I need to move out! Maybe we'll meet again, someday...")
        (setq on-dialog-closed exit)))))
