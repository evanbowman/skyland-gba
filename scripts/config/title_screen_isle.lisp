;;;
;;; title_screen_isle.lisp
;;;
;;; The title screen islands are defined in title/. The game invokes this script
;;; in the macrocosm room of the tile screen, and selects a random layout from
;;; the title/ subdirectory. When adding new islands, modify the choice count
;;; below.
;;;


(foreach
 (lambda (vxyz)
   (mcr-block-set
    (get vxyz 1)
    (get vxyz 2)
    (get vxyz 3)
    (get vxyz 0)))
 (eval-file (format "/scripts/config/title/%.lisp" (choice 4))))
