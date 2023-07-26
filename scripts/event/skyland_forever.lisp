;;;
;;; skyland_forever.lisp
;;;


(eval-file "/scripts/reset_hooks.lisp")


(terrain (player) 4)

(island-configure
 (player)
 '((power-core 1 13)
   (ladder 0 13)
   (hull 0 12)))


(map (lambda
       (chr-new (player)
                $0
                14
                'neutral
                (list (cons 'icon (sample '(1 5 10 11 16 14))))))
     (range 0 3))

(flag-show (player) 0)


(setq chr-names '())
