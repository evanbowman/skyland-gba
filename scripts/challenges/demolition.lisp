;;
;; demolition.lisp
;;



(defn on-fadein
  (dialog "<c:goblin king:3>This whole island needs to be demolissshed! That'sss right, every ssingle block! Not one block can remain!")
  (defn on-dialog-closed
    (dialog
     "<c:goblin:2>Hey King, we've finished wiring the island with explossivesss!")
    (defn on-dialog-closed
      (dialog
       "<c:goblin king:3>Heh. Well this complicates things... hahahahaha")
      (setq on-dialog-closed nil))))


(defn challenge-hint
  (dialog "Are you sure you want a hint?")
  (dialog-await-y/n)

  (defn on-dialog-accepted
    (dialog "Hint: Check which blocks are vulnerable to ion cannons. "
            "The glossary has a search filter for this..."))

  (setq on-dialog-declined (lambda '())))


(let ((skip nil))
  (defn on-victory
    (let ((cnt (+ (rcnt (opponent) 'power-core)
                  (rcnt (opponent) 'reactor))))
      (if (> (length (rooms (opponent))) cnt)
          (progn
            (let ((rem nil)
                  (hint '(")")))
              (map (lambda
                     (if (not (assoc (car $0) rem))
                         (setq rem (cons (cons (car $0) (rcnt (opponent) (car $0))) rem))))
                   (rooms (opponent)))
              (map (lambda
                     (setq hint
                           (cons (string
                                  (string (cdr $0))
                                  " "
                                  (rinfo 'name (car $0))
                                  ",")
                                 hint)))
                   rem)
              (setq hint (cons "(" hint))
              (setq hint (apply string hint))

              (dialog "<c:goblin king:3>NO! WRONG!! "
                      "The island is sssinking and some blockss remain! "
                      hint)
              (setq skip 1)
              (exit 3)))
        (if (not skip)
            (syscall "challenge-complete" 11))))))



(terrain (player) 4)
(island-configure
 (player)
 '((power-core 1 13)
   (manufactory 1 11)))

(flag-show (player) 0)

(coins-add 99999)


(opponent-init 14 'hostile)

(island-configure
 (opponent)
 '((energized-hull 0 13)
   (energized-hull 0 12)
   (energized-hull 0 9)
   (energized-hull 0 10)
   (energized-hull 0 14)
   (energized-hull 0 11)
   (energized-hull 0 8)
   (energized-hull 0 7)
   (masonry 1 11 0)
   (energized-hull 1 13)
   (energized-hull 1 7)
   (energized-hull 1 10)
   (energized-hull 1 14)
   (masonry 1 12 0)
   (energized-hull 1 8)
   (dynamite 1 6)
   (energized-hull 1 9)
   (energized-hull 2 12)
   (energized-hull 2 11)
   (masonry 2 13 0)
   (dynamite 2 7)
   (dynamite 2 6)
   (masonry 2 10 0)
   (dynamite 2 8)
   (masonry 2 14 0)
   (masonry 2 9 0)
   (masonry 1 5)
   (masonry 2 5)
   (masonry 3 5)
   (dynamite 3 6)
   (dynamite 3 8)
   (dynamite 3 7)
   (dynamite 3 12)
   (power-core 3 13)
   (dynamite 3 10)
   (dynamite 3 11)
   (dynamite 3 9)
   (hull 4 11)
   (dynamite 4 12)
   (dynamite 4 10)
   (hull 5 10)
   (dynamite 5 12)
   (hull 5 9)
   (power-core 5 13)
   (dynamite 5 11)
   (hull 6 9)
   (dynamite 6 12)
   (power-core 6 10)
   (power-core 6 7)
   (hull 6 6)
   (dynamite 7 9)
   (hull 7 6)
   (workshop 7 13)
   (hull 7 12)
   (hull 8 7)
   (hull 8 6)
   (dynamite 8 9)
   (hull 8 12)
   (dynamite 8 10)
   (dynamite 8 8)
   (dynamite 8 11)
   (dynamite 9 6)
   (dynamite 9 7)
   (reactor 9 12)
   (power-core 9 10)
   (dynamite 9 8)
   (energized-hull 9 9)
   (energized-hull 10 9)
   (energized-hull 10 7)
   (dynamite 10 8)
   (energized-hull 10 6)
   (dynamite 11 9)
   (energized-hull 11 6)
   (dynamite 11 13)
   (dynamite 11 8)
   (energized-hull 11 7)
   (dynamite 11 12)
   (dynamite 11 11)
   (dynamite 11 10)
   (dynamite-ii 11 14)
   (energized-hull 12 10)
   (energized-hull 12 9)
   (power-core 12 13)
   (power-core 12 11)
   (energized-hull 12 8)
   (energized-hull 12 7)
   (energized-hull 12 6)
   (energized-hull 13 10)))
