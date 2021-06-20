;;;
;;; newgame.lisp
;;;
;;; Invoked when starting a new game.
;;;


(eval-other-file "reset_hooks.lisp")

(set 'last-zone 0)

(set 'enemies-seen '())
(set 'friendlies-seen '())
