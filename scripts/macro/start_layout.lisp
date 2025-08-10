;;;
;;; macro/start_layout.lisp
;;;


(mcr-sector 0 0)

(map
 (lambda (xyz)
   (mcr-block-set
    (get xyz 0)
    (get xyz 1)
    (get xyz 2)
    4))
 '((3 3 0)
   (3 2 0)
   (2 2 0)
   (4 2 0)
   (2 3 0)
   (3 4 0)
   (4 3 0)
   (4 4 0)))

(mcr-block-set 3 3 1 1)
