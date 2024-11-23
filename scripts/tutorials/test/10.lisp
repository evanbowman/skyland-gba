;;;
;;; replicator.lisp
;;;


(assert-eq (chrs (player))
           '((4 14 (hp . 191) (rplc . 1) (sc . 12) (id . 3))
             (3 14 (hp . 143) (rplc . 1) (id . 5))
             (2 14 (hp . 191) (rplc . 1) (sc . 9) (id . 4))
             (1 14 (sc . 18) (id . 1))))

(assert-eq (coins) 17900)
