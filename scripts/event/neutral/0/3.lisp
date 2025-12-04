;;;
;;; neutral/0/3.lisp
;;;


(let ((path "scripts/event/neutral/0/3_%.lisp"))
  (let ((default (format path (faction)))
        (scenarios (map (curry format path) '(human goblin sylph))))
    (eval-file (if (chance (if (equal (faction) 'goblin) 4 2))
                   (sample (filter (notequal? default) scenarios))
                   default))))
