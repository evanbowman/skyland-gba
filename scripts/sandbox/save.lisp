;;;
;;; save.lisp
;;;


(list
 (cons 'save-protocol 3)
 (cons 'rooms (cons (rooms (player)) (rooms (opponent))))
 (cons 'chrs (cons (chrs (player)) (chrs (opponent))))
 (cons 'groups (groups))
 (cons 'weather (weather))
 (cons 'terrain (cons (terrain (player)) (terrain (opponent)))))
