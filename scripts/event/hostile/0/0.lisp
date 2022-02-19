;;;
;;; hostile/0/0.lisp
;;;


(opponent-init 3 'hostile)


(show-flag (opponent))


(dialog
 "Your castle sails through the clouds, buffeted by a cool breeze. "
 "Another fortress approaches, flying a black flag...")


(if (equal (choice 2) 0)
    (opponent-generate 0)
  (island-configure
   (opponent)
   (if (choice 2)
       '((cannon 0 13)
         (hull 0 14)
         (power-core 1 13))
     '((hull 2 12)
       (missile-silo 1 11)
       (power-core 1 13)))))
