;;
;; coord_system.lisp
;;
;; Describes the coordinate
;; system used by SKYLAND's
;; engine.
;;
;; SKYLAND uses an inverted
;; coordinate system
;; consisting of sixteen rows,
;; such that row fifteen
;; includes an island's
;; terrain. e.g.:
;;
;; X: terrain
;;
;; ... 0, 1, 2, 3, 4, 5, 6,...
;; 11:
;; 12:
;; 13:
;; 14:
;; 15: X  X  X  X  X  X
;;
;; Now, let's suppose that a
;; script places a power-core,
;; a 2x2 room, at coordinate
;; (1, 13). Now, our grid
;; looks like this:
;;
;; X: terrain
;; R: room
;;
;; ... 0, 1, 2, 3, 4, 5, 6,...
;; 11:
;; 12:
;; 13:    R  R
;; 14:    R  R
;; 15: X  X  X  X  X  X
;;
;; Makes sense?
;; Long story short, the
;; game places terrain in the
;; highest numbered row of
;; the map grid. X indices
;; increase from left to
;; right, and y indices
;; increase from top to
;; bottom.
