;;;
;;; index.lisp
;;;
;;; (display-name filename type-override)
;;; FIXME: Some types from the QR-encoded base string don't look correct when
;;; decoding with the base32 Linux command line tool. For now, I'm manually
;;; entering the island layout shape (3 == standard, 4 == wider, 5 == superflat).
;;; Obviously there's a logical explanation why stuff doesn't decode correctly,
;;; but I don't have time to look into it now.
;;;


'(("clifftop" "clifftop.dat" 3)
  ("oasis" "oasis.dat" 5)
  ("wind fortress" "wind_fortress.dat" 3)
  ("caldera" "caldera.dat" 3)
  ("mining town" "mining_town.dat" 4)
  ("monolith" "monolith.dat" 3)
  ("ritual shrine" "ritual_shrine.dat" 3)
  ("monastery" "monastery.dat" 3))
