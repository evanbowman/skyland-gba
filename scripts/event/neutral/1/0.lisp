;;;
;;; neutral/1/0.lisp
;;;


(let ((path "scripts/event/neutral/1/0_%.lisp"))
  (let ((default (format path (faction)))
        (scenarios (map (curry format path) '(human goblin sylph))))
    (eval-file (if (chance 3)
                   (sample (filter (notequal? default) scenarios))
                   default))))
