
;; The title screen islands are defined in title/. The game invokes this script
;; in the macrocosm room of the tile screen, and selects a random layout from
;; the title/ subdirectory. When adding new islands, modify the choice count
;; below.

(map
 (lambda
   (mcr-block-set
    (get $0 1)
    (get $0 2)
    (get $0 3)
    (get $0 0)))
 (eval-file (format "/scripts/config/title/%.lisp" (choice 4))))
