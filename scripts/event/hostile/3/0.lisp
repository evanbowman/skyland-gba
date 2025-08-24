;;;
;;; hostile/3/0.lisp
;;;


(opponent-init 10 'hostile)

(island-configure
 (opponent)
 '((reactor 8 12)
   (power-core 6 13)
   (power-core 6 11)
   (forcefield 0 14)
   (forcefield 0 13)
   (forcefield 1 14)
   (forcefield 1 13)
   (forcefield 2 14)
   (forcefield 2 13)
   (hull 4 12)
   (hull 5 12)
   (hull 3 12)
   (hull 2 12)
   (hull 1 12)
   (hull 4 11)
   (hull 3 11)
   (hull 2 11)
   (hull 1 11)
   (hull 5 11)
   (hull 1 10)
   (ion-cannon 2 10)
   (hull 6 10)
   (hull 7 10)
   (hull 8 11)
   (hull 9 11)
   (hull 8 10)
   (hull 9 10)
   (hull 6 9)
   (hull 7 9)
   (hull 5 10)
   (decimator 4 13)))

(secret
 8 10
 "Jnefuvc #1AEM8. Pbzzvffvbarq lrne 2047. Fgnl pyrne bs qrpvzngbe!")

(flag-show (opponent) flag-id-pirate)

(chr-new (opponent) 6 14 'hostile 0)
(chr-new (opponent) 7 14 'hostile 0)
