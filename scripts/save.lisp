;;;
;;; The game invokes this script when creating a save file. The entire contents
;;; of the list will be serialized and saved for later. This file should only
;;; contain a single s-expression, as subsequent expressions will not be
;;; evaluated.
;;;


(list
 (cons 'save-protocol 1)
 (cons 'rooms (rooms (player)))
 (cons 'chrs (chrs (player)))
 (cons 'enemies-seen enemies-seen)
 (cons 'friendlies-seen friendlies-seen)
 (cons 'last-zone last-zone)
 (cons 'terrain (terrain (player))))
