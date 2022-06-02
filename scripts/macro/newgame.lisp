
(coins-set 160)

(mcr-sector 0 0)

(map
 (lambda
   (mcr-block-set
    (get $0 0)
    (get $0 1)
    (get $0 2)
    4))
 '((3 3 0)
   (3 2 0)
   (2 3 0)
   (3 4 0)
   (4 3 0)
   (4 4 0)))

(mcr-block-set 2 2 0 5)
(mcr-block-set 4 2 0 5)
(mcr-block-set 3 3 1 1)
