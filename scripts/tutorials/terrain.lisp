;;;
;;; terrain.lisp
;;;


(coins-add 8000)

(terrain-set (player) 4)
(island-configure
 (player)
 '((power-core 1 13)))


(opponent-init 4 'hostile)

(island-configure
 (opponent)
 '((power-core 1 13)))


(autopilot
 '((2000 nil)
   (100 "<c:Milo:5>Ever need more room to build? You can easily add more terrain to your island, just like this:")
   (801 Right)
   (233 Right)
   (250 Right)
   (283 Right)
   (512 R)
   (450 A)
   (166 A)
   (184 Right)
   (199 A)
   (149 A)
   (200 Right)
   (150 A)
   (199 A)
   (217 Right)
   (166 A)
   (183 A)
   (234 Right)
   (183 A)
   (199 A)
   (417 Right)
   (183 A)
   (250 A)
   (266 Right)
   (935 B)
   (300 B)))
