;;;
;;; hostile/0/3.lisp
;;;


(opponent-init 5 'hostile)


(flag-show (opponent) 0)


(island-configure
 (opponent)
 '((cannon 0 13)
   (missile-silo 1 13)
   (power-core 2 13)
   (stairwell 4 11)))
