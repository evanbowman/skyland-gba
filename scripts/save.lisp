;;;
;;; The game invokes this script when creating a save file. The entire contents
;;; of the list will be string-ified and saved for later. This file should only
;;; contain a single s-expression, as subsequent expressions will not be
;;; evaluated.
;;;

(set 'temp
     (list
      (rooms (player))
      (chrs (player))
      enemies-seen
      friendlies-seen))

(print (string temp))
