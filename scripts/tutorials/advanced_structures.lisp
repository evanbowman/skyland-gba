;;;
;;; advanced_structures.lisp
;;;


(terrain-set (player) 6)
(island-configure
 (player)
 '((power-core 1 13)))


(chr-hp (chr-new (player) 1 14 'neutral 0) 50)


(coins-add 20000)


(opponent-init 4 'hostile)

(island-configure
 (opponent)
 '((power-core 1 13)))


(autopilot
 '((1351 Select)
   (20 A)
   (967 Right)
   (416 Right)
   (400 Right)
   (1933 R)
   (616 A)
   (1500 Right)
   (432 Right)
   (416 Right)
   (400 Right)
   (449 Right)
   (449 Right)
   (499 Right)
   (3099 B)
   (866 B)
   (100 nil)
   (100 "<c:Milo:5>We seem to only be able to build a small set of structures. By constructing a workshop, we unlock a large number of additional rooms. Workshops also reduce construction cost for all new rooms by a bit.")
   (2334 R)
   (1116 A)
   (700 Left)
   (434 Left)
   (434 Left)
   (434 Left)
   (1199 A)
   (949 B)
   (2100 R)
   (1633 A)
   (1517 Right)
   (1034 Right)
   (716 Right)
   (600 Right)
   (633 Right)
   (533 Right)
   (483 Right)
   (449 Right)
   (466 Right)
   (433 Right)
   (516 Right)
   (482 Right)
   (483 Right)
   (1800 B)
   (400 B)))
