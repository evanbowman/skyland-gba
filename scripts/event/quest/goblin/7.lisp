;;;
;;; quest/goblin/7.lisp
;;;


(dialog
 "In the distance, you hear the metallic clang of gears and winches... <B:0>"
 "<b:/scripts/data/img/mining.img.bin>"
 "A mining rig appears! <B:0> "
 "Steam billows from its engines as cranes dredge heavy ore from the surface...")


(opponent-init 12 'neutral)

(island-configure
 (opponent)
 '((canvas 0 9 (40 -1442176000 1215987712 -1324957524 1659748812 1090562054 1488020203 3146496 -1608613867 1071654657 193 254 15 160))
   (hull 0 14)
   (hull 0 13)
   (canvas 0 10 (43 1696994304 -2046783182 1617956358 241440384 1073741977 -1610153307 435716128 -2067742432 1632710820 -536780799 15 0 8))
   (hull 1 8)
   (canvas 1 9 (44 54585004 75541141 -1673923826 453037274 -331343848 -671035379 3212096 -343926768 -59310063 -133832703 130 28 95 224))
   (power-core 1 10)
   (hull 1 14)
   (ladder 1 12)
   (dynamite-ii 2 12)
   (canvas 2 9 (45 54585004 75541141 -1719443442 1624696524 -1027549184 -2123472888 419480198 -457179000 59262984 1040575 -502217681 255))
   (dynamite-ii 2 13)
   (dynamite-ii 2 14)
   (dynamite-ii 3 12)
   (masonry 3 11 0)
   (dynamite-ii 3 14)
   (canvas 3 9 (56 836960255 -2146957287 1745879654 306563715 1679302656 -930024384 253825408 109092000 -2092953571 -239075120 532981535 -1846800257 -2121920892 0 64 61 0))
   (canvas 3 8 (57 -642563200 1074034943 2071989773 99680189 -237023357 -1107245055 -1677260256 678494276 1080298640 -2001467615 135893504 73695549 -2079688096 109223998 136))
   (dynamite-ii 3 13)
   (bridge 3 10)
   (dynamite-ii 4 14)
   (dynamite-ii 4 13)
   (hull 4 9)
   (dynamite-ii 4 12)
   (masonry 4 11 0)
   (masonry 5 11 0)
   (dynamite-ii 5 12)
   (dynamite-ii 5 14)
   (hull 5 9)
   (bridge 5 10)
   (dynamite-ii 5 13)
   (dynamite-ii 6 14)
   (masonry 6 11 0)
   (dynamite-ii 6 13)
   (dynamite-ii 6 12)
   (dynamite-ii 7 14)
   (dynamite-ii 7 13)
   (bridge 7 10)
   (dynamite-ii 7 12)
   (masonry 7 11 0)
   (dynamite-ii 8 14)
   (masonry 8 11 0)
   (dynamite-ii 8 12)
   (dynamite-ii 8 13)
   (hull 8 9)
   (masonry 9 11 0)
   (bridge 9 10)
   (dynamite-ii 9 13)
   (crane 9 8)
   (dynamite-ii 9 12)
   (dynamite-ii 9 14)
   (hull 10 7)
   (masonry 10 13 0)
   (masonry 10 11 0)
   (masonry 10 12 0)
   (hull 10 14)
   (hull 11 14)
   (hull 11 13)))


(defn on-converge ()
  (setq on-converge nil)
  (dialog "<c:Mining Chief:20>Hey there! One of our other mining platforms nearby is running low on blasting equipment. <B:0> Can you do us a favor and transport some explosives for us?")
  (defn on-dialog-closed ()
    (dialog "Sounds extremely dangerous... but the miners offer to pay you quite well. Accept task?")
    (dialog-await-y/n))

  (defn on-dialog-accepted ()
    (let ((m (eval-file "/scripts/event/quest/make_quest_marker.lisp"))
          (bloc (if (equal (difficulty) difficulty-beginner)
                    'dynamite
                  'dynamite-ii)))
      (if m
          (progn
            ;; push adventure log
            (push 'qids 7)
            (push 'quests (cons "dynamite-ii.lisp" m))
            (dialog "<c:Mining Chief:20>Great! Let's move some of this cargo over to your island...")
            (adventure-log-add 57 nil)

            (defn on-dialog-closed ()
              (let ((cnt 0)) ;; closure
                ((lambda ()
                   (let ((t (this))
                         (del 0))
                     (alloc-space bloc)
                     (sel-input
                      bloc
                      (string "Place explosive " (+ cnt 1) "/7:")
                      (lambda (isle x y)
                        (room-new (player) (list bloc x y))
                        (sound "build0")

                        (room-del (opponent) (+ cnt 2) 12)

                        (+= cnt 1)

                        (if (equal cnt 7)
                            (progn
                              (dialog "<c:Mining Chief:20>Here, I'll mark the delivery location on your map with an *. Be careful, OK? <B:0> This stuff is extremely volatile. (hover to see blast radius) <B:0> The slightest damage and it'll make an explosion like you've never seen!")
                              (setq on-dialog-closed exit))
                          (t))))))))))
        (progn
          (dialog "<c:Mining Chief:20>Unfortunately, we just got a report from our other mining rig that bad weather will make the delivery impossible. Thanks anyway!")
          (exit)))))

  (setq on-dialog-declined exit))
