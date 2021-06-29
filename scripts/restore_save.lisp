;;;
;;; The game invokes this script when loading a save file. The script should
;;; contain a single s-expression, a lambda, which will receive data written by
;;; save.lisp.
;;;


(eval-other-file "reset_hooks.lisp")


(lambda
  (configure-player
   (player)
   (get (arg 0) 0)) ;; list of rooms in list index zero

  (map
   (lambda
     (add-chr (player) (car (arg 0)) (cdr (arg 0))))
   (get (arg 0) 1)) ;; list of characters in list index one

  (set 'enemies-seen (get (arg 0) 2))
  (set 'frendlies-seen (get (arg 0) 3)))
