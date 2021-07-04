;;;
;;; The game invokes this script when creating a save file. The entire contents
;;; of the list will be serialized and saved for later. This file should only
;;; contain a single s-expression, as subsequent expressions will not be
;;; evaluated.
;;;


(list
 (rooms (player))
 (chrs (player))
 enemies-seen
 friendlies-seen
 last-zone
 (terrain (player))
 0 ;; save protocol version.
 )
